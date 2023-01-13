use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while},
    character::complete::{
        char, digit1, hex_digit1, i8, multispace1, one_of, satisfy, space0, space1, u8,
    },
    combinator::{fail, map, map_opt, map_res, opt, peek, recognize, success, value},
    multi::{many0, many1, many_till, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use regex::Regex;
use sleigh_types::ast::*;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

fn identifier_ref(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        satisfy(|c| c == '_' || c == '.' || c.is_ascii_alphabetic()),
        take_while(|c: char| c == '_' || c == '.' || c.is_ascii_alphanumeric()),
    ))(input)
}

fn identifier_no_ws(input: &str) -> IResult<&str, String> {
    map(identifier_ref, ToOwned::to_owned)(input)
}

fn identifier(input: &str) -> IResult<&str, String> {
    ws(identifier_no_ws)(input)
}

fn string_ref(input: &str) -> IResult<&str, &str> {
    let q = '"';
    delimited(char(q), take_till(move |c| c == q), char(q))(input)
}

fn preprocessor_value(input: &str) -> IResult<&str, &str> {
    alt((string_ref, digit1))(input)
}

fn string_no_ws(input: &str) -> IResult<&str, String> {
    map(string_ref, ToOwned::to_owned)(input)
}

fn string(input: &str) -> IResult<&str, String> {
    ws(string_no_ws)(input)
}

fn preprocess_expression_atom<'a>(
    input: &'a str,
    defs: &HashMap<String, String>,
) -> IResult<&'a str, bool> {
    alt((
        map(
            delimited(tag("defined("), identifier_ref, char(')')),
            |id| defs.contains_key(id),
        ),
        delimited(
            char('('),
            |input| preprocess_expression(input, defs),
            char(')'),
        ),
        map(
            separated_pair(
                identifier_ref,
                tuple((space0, tag("=="), space0)),
                string_ref,
            ),
            |(id, s)| defs[id] == s,
        ),
    ))(input)
}

fn preprocess_expression<'a>(
    input: &'a str,
    defs: &HashMap<String, String>,
) -> IResult<&'a str, bool> {
    op_prec(
        &|input| preprocess_expression_atom(input, defs),
        &|op: fn(bool, bool) -> bool, l, r| op(l, r),
        0,
        &OpTable::new(&[&[("||", |l, r| l || r)], &[("&&", |l, r| l && r)]]),
        input,
    )
}

// Assume we are at the start of the line
fn preprocess_directive<'a>(
    input: &'a str,
    defs: &HashMap<String, String>,
) -> IResult<&'a str, PreProcessDirective<'a>> {
    use PreProcessDirective::*;
    preceded(
        char('@'),
        alt((
            map(
                preceded(pair(tag("include"), space1), string_ref),
                |file_path| Include { file_path },
            ),
            map(
                preceded(
                    pair(tag("define"), space1),
                    separated_pair(identifier_ref, space1, preprocessor_value),
                ),
                |(var_name, value)| Define { var_name, value },
            ),
            map(
                preceded(pair(tag("undef"), space1), identifier_ref),
                |var_name| Undef { var_name },
            ),
            map(
                preceded(pair(tag("ifdef"), space1), identifier_ref),
                |var_name| IfDef {
                    condition: defs.contains_key(var_name),
                },
            ),
            map(
                preceded(pair(tag("ifndef"), space1), identifier_ref),
                |var_name| IfNDef {
                    condition: defs.contains_key(var_name),
                },
            ),
            map(
                preceded(pair(tag("if"), space1), |input| {
                    preprocess_expression(input, defs)
                }),
                |condition| If { condition },
            ),
            map(
                preceded(pair(tag("elif"), space1), |input| {
                    preprocess_expression(input, defs)
                }),
                |condition| ElIf { condition },
            ),
            map(tag("endif"), |_| EndIf),
            map(tag("else"), |_| Else),
            map(success(input), |line| Unknown { line }),
        )),
    )(input)
}

fn preprocess_expand_macros<'a>(line: &'a str, defs: &HashMap<String, String>) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\$\(([a-zA-Z0-9_.]+)\)").unwrap();
    }
    RE.replace_all(line, |caps: &regex::Captures| {
        let cap = &caps[1];
        defs.get(cap).expect(cap)
    })
    .into_owned()
}

fn preprocess_internal(
    input: impl BufRead,
    root: &Path,
    out: &mut String,
    defs: &mut HashMap<String, String>,
) {
    let mut state_stack = vec![true];
    for line in input.lines() {
        let line = line.unwrap();
        let current_state = *state_stack.last().unwrap();
        let line = if current_state && !line.starts_with('#') {
            preprocess_expand_macros(&line, defs)
        } else {
            line
        };
        match preprocess_directive(&line, defs) {
            Ok(directive) => match directive.1 {
                PreProcessDirective::Include { file_path } => {
                    let file_path = root.join(file_path);
                    let f = File::open(file_path).unwrap();
                    let r = BufReader::new(f);
                    preprocess_internal(r, root, out, defs);
                }
                PreProcessDirective::Define { var_name, value } => {
                    if current_state {
                        defs.insert(var_name.to_owned(), value.to_owned());
                    }
                }
                PreProcessDirective::Undef { var_name } => {
                    if current_state {
                        defs.remove(var_name);
                    }
                }
                PreProcessDirective::IfDef { condition } => {
                    state_stack.push(current_state && condition);
                }
                PreProcessDirective::IfNDef { condition } => {
                    state_stack.push(current_state && !condition);
                }
                PreProcessDirective::If { condition } => {
                    state_stack.push(current_state && condition);
                }
                PreProcessDirective::ElIf { condition } => {
                    state_stack.pop().unwrap();
                    state_stack.push(!current_state && condition);
                }
                PreProcessDirective::EndIf => {
                    state_stack.pop().unwrap();
                }
                PreProcessDirective::Else => {
                    state_stack.pop().unwrap();
                    state_stack.push(!current_state);
                }
                PreProcessDirective::Unknown { line } => {
                    panic!("unknown preprocess directive: {line}")
                }
            },
            Err(_) => {
                if current_state {
                    out.push_str(&line);
                    out.push('\n');
                }
            }
        }
    }
}

pub fn preprocess(input: impl BufRead, root: &Path) -> String {
    let mut ret = String::new();
    let mut defs = HashMap::new();
    preprocess_internal(input, root, &mut ret, &mut defs);
    ret
}

pub fn parse_sleigh(input: &str) -> IResult<&str, Sleigh> {
    map(many0(parse_def), |defs| Sleigh { defs })(input)
}

fn parse_endian_def(input: &str) -> IResult<&str, EndianDef> {
    map(
        delimited(
            tuple((tok("define"), tok("endian"), tok("="))),
            ws(parse_endian),
            tok(";"),
        ),
        |endian| EndianDef { endian },
    )(input)
}

fn parse_align_def(input: &str) -> IResult<&str, AlignDef> {
    map(
        delimited(
            tuple((tok("define"), tok("alignment"), tok("="))),
            ws(u8),
            tok(";"),
        ),
        |align| AlignDef { align },
    )(input)
}

fn parse_space_def(input: &str) -> IResult<&str, SpaceDef> {
    map(
        tuple((
            preceded(pair(tok("define"), tok("space")), identifier),
            preceded(pair(tok("type"), tok("=")), parse_space_type),
            preceded(pair(tok("size"), tok("=")), ws(u8)),
            terminated(map(opt(tok("default")), |d| d.is_some()), tok(";")),
        )),
        |(name, typ, size, default)| SpaceDef {
            name,
            typ,
            size,
            default,
        },
    )(input)
}

fn parse_space_type(input: &str) -> IResult<&str, SpaceType> {
    alt((
        value(SpaceType::Ram, tok("ram_space")),
        value(SpaceType::Register, tok("register_space")),
    ))(input)
}

fn parse_endian(input: &str) -> IResult<&str, Endian> {
    alt((
        value(Endian::Big, tok("big")),
        value(Endian::Little, tok("little")),
    ))(input)
}

fn parse_base(input: &str) -> IResult<&str, Base> {
    alt((
        map(tok("hex"), |_| Base::Hex),
        map(tok("dec"), |_| Base::Dec),
    ))(input)
}

fn parse_token_parent_info(input: &str) -> IResult<&str, TokenParentInfo> {
    map(
        preceded(
            pair(tok("define"), tok("token")),
            tuple((
                identifier,
                map_opt(delimited(char('('), ws(u8), char(')')), |size| {
                    if size % 8 == 0 {
                        Some(size / 8)
                    } else {
                        None
                    }
                }),
                opt(preceded(pair(tok("endian"), tok("=")), parse_endian)),
            )),
        ),
        |(name, size, endian)| TokenParentInfo { name, size, endian },
    )(input)
}

fn parse_token_def(input: &str) -> IResult<&str, TokenDef> {
    map(
        terminated(
            pair(parse_token_parent_info, many1(parse_field_def)),
            tok(";"),
        ),
        |(info, fields)| TokenDef { info, fields },
    )(input)
}

fn parse_field_def(input: &str) -> IResult<&str, FieldDef> {
    map(
        tuple((
            terminated(identifier, tok("=")),
            delimited(tok("("), separated_pair(ws(u8), tok(","), ws(u8)), tok(")")),
            map(opt(tok("signed")), |f| f.is_some()),
            opt(parse_base),
        )),
        |(name, (low, high), signed, base)| FieldDef {
            name,
            low,
            high,
            signed,
            base,
        },
    )(input)
}

fn parse_context_field_def(input: &str) -> IResult<&str, ContextFieldDef> {
    map(
        tuple((
            terminated(identifier, tok("=")),
            delimited(tok("("), separated_pair(ws(u8), tok(","), ws(u8)), tok(")")),
            map(opt(tok("signed")), |f| f.is_some()),
            map(opt(tok("noflow")), |f| f.is_some()),
            opt(parse_base),
        )),
        |(name, (low, high), signed, noflow, base)| ContextFieldDef {
            name,
            low,
            high,
            signed,
            noflow,
            base,
        },
    )(input)
}

fn parse_context_def(input: &str) -> IResult<&str, ContextDef> {
    map(
        delimited(
            pair(tok("define"), tok("context")),
            pair(identifier, many1(parse_context_field_def)),
            tok(";"),
        ),
        |(var_node, fields)| ContextDef { var_node, fields },
    )(input)
}

fn int_b_list(input: &str) -> IResult<&str, Vec<i8>> {
    alt((
        map(i8, |i| vec![i]),
        delimited(tok("["), many1(ws(i8)), tok("]")),
    ))(input)
}

fn identifier_list(input: &str) -> IResult<&str, Vec<String>> {
    alt((
        map(identifier, |name| vec![name]),
        delimited(tok("["), many1(identifier), tok("]")),
    ))(input)
}

fn any_string_list(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tok("["), many1(alt((string, identifier))), tok("]"))(input)
}

fn parse_var_node_def(input: &str) -> IResult<&str, VarNodeDef> {
    map(
        tuple((
            preceded(tok("define"), identifier),
            preceded(pair(tok("offset"), tok("=")), parse_u64),
            preceded(pair(tok("size"), tok("=")), parse_u64),
            terminated(identifier_list, tok(";")),
        )),
        |(space_sym, offset, size, names)| VarNodeDef {
            space_sym,
            offset,
            size,
            names,
        },
    )(input)
}

fn parse_p_code_op_def(input: &str) -> IResult<&str, PCodeOpDef> {
    map(
        delimited(
            pair(tok("define"), tok("pcodeop")),
            identifier_list,
            tok(";"),
        ),
        |ops| PCodeOpDef { ops },
    )(input)
}

fn parse_value_attach(input: &str) -> IResult<&str, ValueAttach> {
    map(
        delimited(
            pair(tok("attach"), tok("values")),
            pair(identifier_list, int_b_list),
            tok(";"),
        ),
        |(fields, values)| ValueAttach { fields, values },
    )(input)
}

fn parse_name_attach(input: &str) -> IResult<&str, NameAttach> {
    map(
        delimited(
            pair(tok("attach"), tok("names")),
            pair(identifier_list, any_string_list),
            tok(";"),
        ),
        |(fields, names)| NameAttach { fields, names },
    )(input)
}

fn parse_var_attach(input: &str) -> IResult<&str, VarAttach> {
    map(
        delimited(
            pair(tok("attach"), tok("variables")),
            pair(identifier_list, identifier_list),
            tok(";"),
        ),
        |(fields, registers)| VarAttach { fields, registers },
    )(input)
}

fn parse_definition(input: &str) -> IResult<&str, Definition> {
    alt((
        map(parse_token_def, Definition::TokenDef),
        map(parse_context_def, Definition::ContextDef),
        map(parse_space_def, Definition::SpaceDef),
        map(parse_var_node_def, Definition::VarNodeDef),
        map(parse_p_code_op_def, Definition::PCodeOpDef),
        map(parse_value_attach, Definition::ValueAttach),
        map(parse_name_attach, Definition::NameAttach),
        map(parse_var_attach, Definition::VarAttach),
    ))(input)
}

fn parse_display_token(input: &str) -> IResult<&str, DisplayToken> {
    alt((
        value(DisplayToken::Caret, char('^')),
        map(string_no_ws, DisplayToken::String),
        map(
            one_of("~!@#$%&*()-=+[]{}|;:<>?,/0123456789"),
            DisplayToken::Char,
        ),
        value(DisplayToken::Space, multispace1),
        map(identifier_no_ws, DisplayToken::Symbol),
    ))(input)
}

fn parse_display_section(input: &str) -> IResult<&str, DisplaySection> {
    map(many_till(parse_display_token, tok("is")), |(toks, _)| {
        DisplaySection { toks }
    })(input)
}

fn parse_constructor(input: &str) -> IResult<&str, Constructor<()>> {
    map(
        tuple((
            map(terminated(opt(identifier), char(':')), |name| {
                name.unwrap_or_else(|| INSTRUCTION.to_owned())
            }),
            parse_display_section,
            parse_pattern_equation,
            parse_context_block,
            parse_rtl_body,
        )),
        |(header, display, p_equation, context_block, rtl_body)| Constructor {
            header,
            display,
            p_equation,
            context_block,
            rtl_body,
        },
    )(input)
}

fn parse_p_expression_unary_op(input: &str) -> IResult<&str, PExpressionUnaryOp> {
    alt((
        value(PExpressionUnaryOp::Minus, tok("-")),
        value(PExpressionUnaryOp::Not, tok("~")),
    ))(input)
}

fn parse_p_expression_unary(input: &str) -> IResult<&str, PExpressionUnary> {
    map(
        pair(parse_p_expression_unary_op, parse_p_expression_atom),
        |(op, operand)| PExpressionUnary { op, operand },
    )(input)
}

fn parse_p_expression_atom(input: &str) -> IResult<&str, PExpression> {
    alt((
        map(parse_u64, PExpression::ConstantValue),
        map(identifier, PExpression::Symbol),
        delimited(tok("("), |input| parse_p_expression(0, input), tok(")")),
        map(parse_p_expression_unary, |e| {
            PExpression::Unary(Box::new(e))
        }),
    ))(input)
}

fn parse_p_expression(min_prec: usize, input: &str) -> IResult<&str, PExpression> {
    op_prec(
        &parse_p_expression_atom,
        &|op, l, r| PExpression::Bin(Box::new(PExpressionBin { op, l, r })),
        min_prec,
        &OpTable::new(&[
            &[("$or", PExpressionBinOp::Or), ("|", PExpressionBinOp::Or)],
            &[
                ("$xor", PExpressionBinOp::Xor),
                ("^", PExpressionBinOp::Xor),
            ],
            &[
                ("$and", PExpressionBinOp::And),
                ("&", PExpressionBinOp::And),
            ],
            &[
                ("<<", PExpressionBinOp::LeftShift),
                (">>", PExpressionBinOp::RightShift),
            ],
            &[("+", PExpressionBinOp::Add), ("-", PExpressionBinOp::Sub)],
            &[("*", PExpressionBinOp::Mult), ("/", PExpressionBinOp::Div)],
        ]),
        input,
    )
}

fn parse_constraint_compare_op(input: &str) -> IResult<&str, ConstraintCompareOp> {
    alt((
        value(ConstraintCompareOp::Equal, tok("=")),
        value(ConstraintCompareOp::NotEqual, tok("!=")),
        value(ConstraintCompareOp::Less, tok("<")),
        value(ConstraintCompareOp::LessEqual, tok("<=")),
        value(ConstraintCompareOp::Greater, tok(">")),
        value(ConstraintCompareOp::GreaterEqual, tok(">=")),
    ))(input)
}

fn parse_constraint_compare(input: &str) -> IResult<&str, ConstraintCompare> {
    map(
        tuple((identifier, parse_constraint_compare_op, |input| {
            parse_p_expression(3, input)
        })),
        |(symbol, op, expr)| ConstraintCompare { op, symbol, expr },
    )(input)
}

fn parse_constraint(input: &str) -> IResult<&str, Constraint> {
    alt((
        map(parse_constraint_compare, Constraint::Compare),
        map(identifier, Constraint::Symbol),
    ))(input)
}

fn parse_atomic(input: &str) -> IResult<&str, Atomic<()>> {
    alt((
        map(parse_constraint, Atomic::Constraint),
        map(
            delimited(tok("("), parse_pattern_equation, tok(")")),
            Atomic::Parenthesized,
        ),
    ))(input)
}

fn parse_ell_rt(input: &str) -> IResult<&str, EllRt<()>> {
    map(
        pair(parse_atomic, parse_ellipsis),
        |(atomic, ellipsis_right)| EllRt {
            atomic,
            ellipsis_right,
        },
    )(input)
}

fn parse_ell_eq(input: &str) -> IResult<&str, EllEq<()>> {
    map(
        pair(parse_ellipsis, parse_ell_rt),
        |(ellipsis_left, ell_rt)| EllEq {
            ellipsis_left,
            ell_rt,
        },
    )(input)
}

fn parse_ellipsis(input: &str) -> IResult<&str, bool> {
    map(opt(tok("...")), |o| o.is_some())(input)
}

fn parse_pattern_equation_ell_eq(input: &str) -> IResult<&str, PatternEquation<()>> {
    map(parse_ell_eq, |e| PatternEquation {
        inner: PatternEquationInner::EllEq(Box::new(e)),
        type_data: (),
    })(input)
}

fn parse_pattern_equation(input: &str) -> IResult<&str, PatternEquation<()>> {
    op_prec(
        &parse_pattern_equation_ell_eq,
        &|op, l, r| PatternEquation {
            inner: PatternEquationInner::Bin(Box::new(PatternEquationBin { op, l, r })),
            type_data: (),
        },
        0,
        &OpTable::new(&[
            &[("|", PatternEquationBinOp::Or)],
            &[(";", PatternEquationBinOp::Cat)],
            &[("&", PatternEquationBinOp::And)],
        ]),
        input,
    )
}

fn parse_context_block(input: &str) -> IResult<&str, ContextBlock> {
    map(
        opt(delimited(
            tok("["),
            many0(parse_context_list_item),
            tok("]"),
        )),
        |o| ContextBlock {
            context_list: o.unwrap_or_default(),
        },
    )(input)
}

fn parse_context_list_item(input: &str) -> IResult<&str, ContextListItem> {
    terminated(
        alt((
            map(
                separated_pair(identifier, tok("="), |input| parse_p_expression(0, input)),
                |(s, e)| ContextListItem::Eq(s, e),
            ),
            map(
                preceded(
                    tok("globalset"),
                    delimited(
                        tok("("),
                        separated_pair(identifier, tok(","), identifier),
                        tok(")"),
                    ),
                ),
                |(s1, s2)| ContextListItem::Set(s1, s2),
            ),
        )),
        tok(";"),
    )(input)
}

fn parse_rtl_body(input: &str) -> IResult<&str, RtlBody> {
    alt((
        map(
            delimited(tok("{"), parse_rtl, tok("}")),
            RtlBody::StandaloneSection,
        ),
        map(
            delimited(tok("{"), pair(parse_rtl_continue, parse_rtl_mid), tok("}")),
            |(c, m)| RtlBody::FinalNamedSection(c, m),
        ),
        map(tok("unimpl"), |_| RtlBody::OpUnimpl),
    ))(input)
}

fn parse_rtl(input: &str) -> IResult<&str, Rtl> {
    map(
        pair(parse_rtl_mid, opt(parse_rtl_export)),
        |(mid, export)| Rtl { mid, export },
    )(input)
}

fn parse_rtl_export(input: &str) -> IResult<&str, RtlExport> {
    delimited(
        tok("export"),
        alt((
            map(parse_export_var_node, RtlExport::ExportVarNode),
            map(pair(parse_sized_star, identifier), |(ss, s)| {
                RtlExport::SizedStar(ss, s)
            }),
        )),
        tok(";"),
    )(input)
}

fn parse_export_var_node(input: &str) -> IResult<&str, ExportVarNode> {
    alt((
        map(identifier, |name| ExportVarNode::Symbol { name }),
        map(
            separated_pair(parse_u64, tok(":"), parse_u64),
            |(i1, i2)| ExportVarNode::Tpl { i1, i2 },
        ),
    ))(input)
}

fn parse_rtl_continue(input: &str) -> IResult<&str, RtlContinue> {
    fail(input)
}

fn parse_rtl_mid(input: &str) -> IResult<&str, RtlMid> {
    map(many0(parse_rtl_mid_item), |items| RtlMid { items })(input)
}

fn parse_rtl_mid_item(input: &str) -> IResult<&str, RtlMidItem> {
    alt((
        map(parse_statement, RtlMidItem::Statement),
        map(
            delimited(
                tok("local"),
                pair(identifier, opt(preceded(tok(":"), ws(u8)))),
                tok(";"),
            ),
            |(s, i)| RtlMidItem::Local(s, i),
        ),
    ))(input)
}

fn parse_sized_star(input: &str) -> IResult<&str, SizedStar> {
    alt((
        map(
            separated_pair(
                delimited(pair(tok("*"), tok("[")), identifier, tok("]")),
                tok(":"),
                ws(u8),
            ),
            |(s, i)| SizedStar::S3(s, i),
        ),
        map(
            delimited(pair(tok("*"), tok("[")), identifier, tok("]")),
            SizedStar::S2,
        ),
        map(preceded(pair(tok("*"), tok(":")), ws(u8)), SizedStar::S1),
        map(tok("*"), |_| SizedStar::S0),
    ))(input)
}

fn parse_statement(input: &str) -> IResult<&str, Statement> {
    alt((
        map(
            terminated(
                separated_pair(
                    pair(identifier, opt(preceded(tok(":"), ws(u8)))),
                    tok("="),
                    parse_expr,
                ),
                tok(";"),
            ),
            |((s, i), e)| Statement::Assign(s, i, e),
        ),
        map(
            delimited(
                tok("local"),
                separated_pair(
                    pair(identifier, opt(preceded(tok(":"), ws(u8)))),
                    tok("="),
                    parse_expr,
                ),
                tok(";"),
            ),
            |((s, i), e)| Statement::LocalAssign(s, i, e),
        ),
        map(
            terminated(
                pair(
                    parse_sized_star,
                    separated_pair(parse_expr, tok("="), parse_expr),
                ),
                tok(";"),
            ),
            |(ss, (e1, e2))| Statement::Store(ss, e1, e2),
        ),
        map(
            terminated(
                pair(
                    identifier,
                    delimited(tok("("), separated_list0(tok(","), parse_expr), tok(")")),
                ),
                tok(";"),
            ),
            |(func, args)| Statement::MiscCall(func, args),
        ),
        map(
            terminated(
                separated_pair(
                    pair(
                        identifier,
                        delimited(tok("["), separated_pair(ws(u8), tok(","), ws(u8)), tok("]")),
                    ),
                    tok("="),
                    parse_expr,
                ),
                tok(";"),
            ),
            |((s, (i1, i2)), e)| Statement::AssignBitRange(s, i1, i2, e),
        ),
        map(
            delimited(tok("build"), identifier, tok(";")),
            Statement::Build,
        ),
        map(
            delimited(
                tok("crossbuild"),
                separated_pair(identifier, tok(","), identifier),
                tok(";"),
            ),
            |(s1, s2)| Statement::CrossBuild(s1, s2),
        ),
        map(
            delimited(
                tok("delayslot"),
                delimited(tok("("), ws(u8), tok(")")),
                tok(";"),
            ),
            Statement::DelaySlot,
        ),
        map(
            delimited(tok("goto"), parse_jump_dest, tok(";")),
            Statement::GoTo,
        ),
        map(
            terminated(
                pair(
                    preceded(tok("if"), parse_expr),
                    preceded(tok("goto"), parse_jump_dest),
                ),
                tok(";"),
            ),
            |(c, j)| Statement::IfGoTo(c, j),
        ),
        map(
            delimited(
                tok("goto"),
                delimited(tok("["), parse_expr, tok("]")),
                tok(";"),
            ),
            Statement::GoToInd,
        ),
        map(
            delimited(tok("call"), parse_jump_dest, tok(";")),
            Statement::Call,
        ),
        map(
            delimited(
                tok("call"),
                delimited(tok("["), parse_expr, tok("]")),
                tok(";"),
            ),
            Statement::CallInd,
        ),
        map(
            delimited(
                tok("return"),
                delimited(tok("["), parse_expr, tok("]")),
                tok(";"),
            ),
            Statement::Return,
        ),
        map(parse_label, Statement::Label),
    ))(input)
}

fn parse_jump_dest(input: &str) -> IResult<&str, JumpDest> {
    alt((
        map(parse_label, JumpDest::Label),
        map(identifier, JumpDest::Symbol),
    ))(input)
}

fn parse_label(input: &str) -> IResult<&str, Label> {
    map(delimited(tok("<"), identifier, tok(">")), |name| Label {
        name,
    })(input)
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    op_prec(
        &parse_expr_atom,
        &|op, l, r| Expr::Bin(Box::new(ExprBin { op, l, r })),
        0,
        &OpTable::new(&[
            &[("||", ExprBinOp::BoolOr)],
            &[("&&", ExprBinOp::BoolAnd), ("^^", ExprBinOp::BoolXor)],
            &[("|", ExprBinOp::IntOr)],
            &[("^", ExprBinOp::IntXor)],
            &[("&", ExprBinOp::IntAnd)],
            &[
                ("==", ExprBinOp::IntEqual),
                ("!=", ExprBinOp::IntNotEqual),
                ("f==", ExprBinOp::FloatEqual),
                ("f!=", ExprBinOp::FloatNotEqual),
            ],
            &[
                ("<", ExprBinOp::IntLess),
                (">", ExprBinOp::IntGreat),
                (">=", ExprBinOp::IntGreatEqual),
                ("<=", ExprBinOp::IntLessEqual),
                ("s<", ExprBinOp::IntSLess),
                ("s>=", ExprBinOp::IntSGreatEqual),
                ("s<=", ExprBinOp::IntSLessEqual),
                ("s>", ExprBinOp::IntSGreat),
                ("f<", ExprBinOp::FloatLess),
                ("f>", ExprBinOp::FloatGreat),
                ("f<=", ExprBinOp::FloatLessEqual),
                ("f>=", ExprBinOp::FloatGreatEqual),
            ],
            &[
                ("<<", ExprBinOp::IntLeft),
                (">>", ExprBinOp::IntRight),
                ("s>>", ExprBinOp::IntSRight),
            ],
            &[
                ("+", ExprBinOp::IntAdd),
                ("-", ExprBinOp::IntSub),
                ("f+", ExprBinOp::FloatAdd),
                ("f-", ExprBinOp::FloatSub),
            ],
            &[
                ("*", ExprBinOp::IntMult),
                ("/", ExprBinOp::IntDiv),
                ("%", ExprBinOp::IntRem),
                ("s/", ExprBinOp::IntSDiv),
                ("s%", ExprBinOp::IntSRem),
                ("f*", ExprBinOp::FloatMult),
                ("f/", ExprBinOp::FloatDiv),
            ],
        ]),
        input,
    )
}

fn parse_expr_func_1_name(input: &str) -> IResult<&str, ExprFunc1Name> {
    alt((
        value(ExprFunc1Name::FloatAbs, tok("abs")),
        value(ExprFunc1Name::FloatSqrt, tok("sqrt")),
        value(ExprFunc1Name::IntSext, tok("sext")),
        value(ExprFunc1Name::IntZext, tok("zext")),
        value(ExprFunc1Name::FloatFloatToFloat, tok("float2float")),
        value(ExprFunc1Name::FloatIntToFloat, tok("int")),
        value(ExprFunc1Name::FloatNan, tok("nan")),
        value(ExprFunc1Name::FloatTrunc, tok("trunc")),
        value(ExprFunc1Name::FloatCeil, tok("ceil")),
        value(ExprFunc1Name::FloatFloor, tok("floor")),
        value(ExprFunc1Name::FloatRound, tok("round")),
        value(ExprFunc1Name::New, tok("new")),
        value(ExprFunc1Name::PopCount, tok("popcount")),
    ))(input)
}

fn parse_expr_func_2_name(input: &str) -> IResult<&str, ExprFunc2Name> {
    alt((
        value(ExprFunc2Name::IntCarry, tok("carry")),
        value(ExprFunc2Name::IntSCarry, tok("scarry")),
        value(ExprFunc2Name::IntSBorrow, tok("sborrow")),
        value(ExprFunc2Name::New, tok("new")),
    ))(input)
}

fn parse_expr_func_1(input: &str) -> IResult<&str, ExprFunc1> {
    map(
        pair(
            parse_expr_func_1_name,
            delimited(tok("("), parse_expr, tok(")")),
        ),
        |(name, arg)| ExprFunc1 { name, arg },
    )(input)
}

fn parse_expr_func_2(input: &str) -> IResult<&str, ExprFunc2> {
    map(
        pair(
            parse_expr_func_2_name,
            delimited(
                tok("("),
                separated_pair(parse_expr, tok(","), parse_expr),
                tok(")"),
            ),
        ),
        |(name, (arg1, arg2))| ExprFunc2 { name, arg1, arg2 },
    )(input)
}

fn parse_expr_unary_op(input: &str) -> IResult<&str, ExprUnaryOp> {
    alt((
        value(ExprUnaryOp::IntTwoComp, tok("-")),
        value(ExprUnaryOp::IntNegate, tok("~")),
        value(ExprUnaryOp::BoolNegate, tok("!")),
        value(ExprUnaryOp::FloatNeg, tok("f-")),
    ))(input)
}

fn parse_expr_unary(input: &str) -> IResult<&str, ExprUnary> {
    map(
        pair(parse_expr_unary_op, parse_expr_atom),
        |(op, operand)| ExprUnary { op, operand },
    )(input)
}

fn parse_expr_atom(input: &str) -> IResult<&str, Expr> {
    alt((
        delimited(tok("("), parse_expr, tok(")")),
        map(pair(parse_sized_star, parse_expr_atom), |(ss, e)| {
            Expr::SizedStar(ss, Box::new(e))
        }),
        map(parse_expr_unary, |e| Expr::Unary(Box::new(e))),
        map(parse_expr_func_1, |e| Expr::Func1(Box::new(e))),
        map(parse_expr_func_2, |e| Expr::Func2(Box::new(e))),
        map(separated_pair(identifier, tok(":"), ws(u8)), |(s, i)| {
            Expr::BitRange1(s, i)
        }),
        map(
            pair(
                identifier,
                delimited(tok("["), separated_pair(ws(u8), tok(","), ws(u8)), tok("]")),
            ),
            |(s, (i1, i2))| Expr::BitRange2(s, i1, i2),
        ),
        map(
            pair(
                identifier,
                delimited(tok("("), separated_list0(tok(","), parse_expr), tok(")")),
            ),
            |(name, args)| Expr::UserOp { name, args },
        ),
        map(parse_var_node, Expr::VarNode),
    ))(input)
}

fn parse_var_node(input: &str) -> IResult<&str, VarNode> {
    alt((
        map(identifier, |name| VarNode::SpecificSymbol { name }),
        map(parse_integer_var_node, |x| {
            VarNode::IntegerVarNode(Box::new(x))
        }),
    ))(input)
}

fn parse_integer_var_node(input: &str) -> IResult<&str, IntegerVarNode> {
    alt((
        map(
            separated_pair(parse_u64, tok(":"), parse_u64),
            |(i1, i2)| IntegerVarNode::S2(i1, i2),
        ),
        map(parse_u64, IntegerVarNode::S1),
        map(
            preceded(pair(tok("&"), tok(":")), pair(parse_u64, parse_var_node)),
            |(i, v)| IntegerVarNode::Address1(i, v),
        ),
        map(preceded(tok("&"), parse_var_node), IntegerVarNode::Address),
    ))(input)
}

fn parse_macro_def(input: &str) -> IResult<&str, MacroDef> {
    map(
        pair(parse_macro_start, delimited(tok("{"), parse_rtl, tok("}"))),
        |(start, rtl)| MacroDef { start, rtl },
    )(input)
}

fn parse_macro_start(input: &str) -> IResult<&str, MacroStart> {
    map(
        preceded(
            tok("macro"),
            pair(
                identifier,
                delimited(tok("("), separated_list0(tok(","), identifier), tok(")")),
            ),
        ),
        |(name, params)| MacroStart { name, params },
    )(input)
}

fn parse_with_block_start(input: &str) -> IResult<&str, WithBlockStart> {
    map(
        delimited(
            tok("with"),
            tuple((
                terminated(opt(identifier), tok(":")),
                opt(parse_pattern_equation),
                parse_context_block,
            )),
            tok("{"),
        ),
        |(id, bitpat, block)| WithBlockStart { id, bitpat, block },
    )(input)
}

fn parse_definition_or_constructorlike(input: &str) -> IResult<&str, DefinitionOrConstructorlike> {
    alt((
        map(parse_definition, DefinitionOrConstructorlike::Definition),
        map(
            parse_constructorlike,
            DefinitionOrConstructorlike::Constructorlike,
        ),
    ))(input)
}

fn parse_with_block_mid(input: &str) -> IResult<&str, WithBlockMid> {
    map(
        pair(
            parse_with_block_start,
            many0(parse_definition_or_constructorlike),
        ),
        |(start, items)| WithBlockMid { start, items },
    )(input)
}

fn parse_with_block(input: &str) -> IResult<&str, WithBlock> {
    map(terminated(parse_with_block_mid, tok("}")), |mid| {
        WithBlock { mid }
    })(input)
}

fn parse_constructorlike(input: &str) -> IResult<&str, Constructorlike> {
    alt((
        map(parse_constructor, Constructorlike::Constructor),
        map(parse_macro_def, Constructorlike::MacroDef),
        map(parse_with_block, Constructorlike::WithBlock),
    ))(input)
}

fn parse_def(input: &str) -> IResult<&str, Def> {
    alt((
        map(parse_endian_def, Def::EndianDef),
        map(parse_align_def, Def::AlignDef),
        map(parse_definition, Def::Definition),
        map(parse_constructorlike, Def::Constructorlike),
    ))(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    value((), pair(char('#'), take_until("\n")))(input)
}

fn spaces_comments(input: &str) -> IResult<&str, ()> {
    value((), many0(alt((value((), multispace1), comment))))(input)
}

fn ws<'a, F: 'a, O>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, nom::error::Error<&'a str>>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, nom::error::Error<&'a str>>,
{
    delimited(spaces_comments, inner, spaces_comments)
}

fn tok<'a>(
    t: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, nom::error::Error<&'a str>> {
    ws(tag(t))
}

fn parse_u64(input: &str) -> IResult<&str, u64> {
    ws(alt((bin_u64, hex_u64, nom::character::complete::u64)))(input)
}

fn bin_u64(input: &str) -> IResult<&str, u64> {
    map_res(preceded(tag("0b"), recognize(many1(one_of("01")))), |out| {
        u64::from_str_radix(out, 0b10)
    })(input)
}

fn hex_u64(input: &str) -> IResult<&str, u64> {
    map_res(preceded(tag("0x"), hex_digit1), |out| {
        u64::from_str_radix(out, 0x10)
    })(input)
}

struct OpTable<O> {
    op_precs: HashMap<&'static str, usize>,
    op_values: HashMap<&'static str, O>,
    op_tags: HashSet<&'static str>,
}

impl<O: Clone> OpTable<O> {
    fn new(bin_ops: &'static [&'static [(&'static str, O)]]) -> Self {
        Self {
            op_precs: bin_ops
                .iter()
                .enumerate()
                .flat_map(|(prec, i)| i.iter().map(move |i| (prec, i)))
                .map(|(prec, (tag, _))| (*tag, prec))
                .collect(),
            op_values: bin_ops.iter().flat_map(|i| i.iter()).cloned().collect(),
            op_tags: bin_ops
                .iter()
                .flat_map(|i| i.iter())
                .map(|(tag, _)| tag)
                .cloned()
                .collect(),
        }
    }

    fn lookup(&self, t: &str) -> Option<&'static str> {
        self.op_tags.get(t).map(|t| *t)
    }
}

fn op_prec<'a, F, B, E, O>(
    e: &F,
    bin_op_func: &B,
    min_prec: usize,
    ops: &OpTable<O>,
    orig_input: &'a str,
) -> IResult<&'a str, E>
where
    O: Clone,
    F: Fn(&'a str) -> IResult<&'a str, E, nom::error::Error<&'a str>>,
    B: Fn(O, E, E) -> E,
{
    fn peek_op<'a, O>(input: &'a str, ops: &OpTable<O>) -> Option<&'static str>
    where
        O: Clone,
    {
        let t = ops
            .op_tags
            .iter()
            .filter_map(|t| peek(tok(t))(input).ok())
            .map(|o| o.1)
            .max_by_key(|t| t.len())?;
        ops.lookup(t)
    }

    fn op_prec_1<'a, F, B, E, O>(
        mut lhs: E,
        min_prec: usize,
        ops: &OpTable<O>,
        e: &F,
        bin_op_func: &B,
        input: &mut &'a str,
    ) -> IResult<&'a str, E>
    where
        O: Clone,
        F: Fn(&'a str) -> IResult<&'a str, E, nom::error::Error<&'a str>>,
        B: Fn(O, E, E) -> E,
    {
        let mut lookahead = peek_op(*input, ops);

        // While lookahead is a binary operator whose precedence is >=
        // min_precedence
        while let Some(op) = lookahead {
            let op_prec = *ops.op_precs.get(op).unwrap();
            let op_valid_prec = op_prec >= min_prec;
            if !op_valid_prec {
                break;
            }

            // Advance to next token
            (*input, _) = tok(op)(*input)?;

            let (new_input, mut rhs) = e(*input)?;
            *input = new_input;

            lookahead = peek_op(*input, ops);

            // While lookahead is a binary operator whose precedence is greater
            // than op's
            while let Some(op2) = lookahead {
                let op2_prec = *ops.op_precs.get(op2).unwrap();
                let op2_valid_prec = op2_prec > op_prec;
                if !op2_valid_prec {
                    break;
                }
                (*input, rhs) = op_prec_1(rhs, op_prec + 1, ops, e, bin_op_func, input)?;
                lookahead = peek_op(*input, ops);
            }
            let op_val = ops.op_values.get(op).unwrap();
            lhs = bin_op_func(op_val.clone(), lhs, rhs);
        }

        Ok((*input, lhs))
    }

    let (mut input, lhs) = e(orig_input)?;
    op_prec_1(lhs, min_prec, ops, e, bin_op_func, &mut input)
}
