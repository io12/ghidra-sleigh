use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
};

use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while},
    character::complete::{
        char, digit1, hex_digit1, i8, multispace1, one_of, satisfy, space0, space1, u8,
    },
    combinator::{fail, map, map_res, opt, peek, recognize, success, value},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use regex::Regex;

#[derive(Debug)]
struct Sleigh {
    defs: Vec<Def>,
}

#[derive(Debug)]
enum Def {
    EndianDef(EndianDef),
    AlignDef(AlignDef),
    Definition(Definition),
    Constructorlike(Constructorlike),
}

#[derive(Debug)]
struct EndianDef {
    endian: Endian,
}

#[derive(Debug, Clone)]
enum Endian {
    Big,
    Little,
}

#[derive(Debug)]
struct AlignDef {
    align: u8,
}

#[derive(Debug)]
enum Definition {
    TokenDef(TokenDef),
    ContextDef(ContextDef),
    SpaceDef(SpaceDef),
    VarNodeDef(VarNodeDef),
    BitRangeDef(BitRangeDef),
    PCodeOpDef(PCodeOpDef),
    ValueAttach(ValueAttach),
    NameAttach(NameAttach),
    VarAttach(VarAttach),
}

#[derive(Debug)]
struct TokenDef {
    name: String,
    i: u8,
    endian: Option<Endian>,
    fields: Vec<FieldDef>,
}

#[derive(Debug)]
struct FieldDef {
    name: String,
    low: u8,
    high: u8,
    signed: bool,
    base: Option<Base>,
}

#[derive(Debug)]
enum Base {
    Hex,
    Dec,
}

#[derive(Debug)]
struct ContextDef {
    var_sym: String,
    fields: Vec<ContextFieldDef>,
}

#[derive(Debug)]
struct ContextFieldDef {
    name: String,
    low: u8,
    high: u8,
    signed: bool,
    noflow: bool,
    base: Option<Base>,
}

#[derive(Debug)]
struct SpaceDef {
    name: String,
    typ: SpaceType,
    size: u8,
    default: bool,
}

#[derive(Debug, Clone)]
enum SpaceType {
    Ram,
    Register,
}

#[derive(Debug)]
struct VarNodeDef {
    space_sym: String,
    offset: u64,
    size: u64,
    names: Vec<String>,
}

#[derive(Debug)]
struct BitRangeDef {
    bit_ranges: Vec<BitRangeSingle>,
}

#[derive(Debug)]
struct BitRangeSingle {
    name: String,
    var_sym: String,
    i1: u8,
    i2: u8,
}

#[derive(Debug)]
struct PCodeOpDef {
    ops: Vec<String>,
}

#[derive(Debug)]
struct ValueAttach {
    value_list: Vec<String>,
    int_b_list: Vec<i8>,
}

#[derive(Debug)]
struct NameAttach {
    value_list: Vec<String>,
    string_list: Vec<String>,
}

#[derive(Debug)]
struct VarAttach {
    value_list: Vec<String>,
    string_list: Vec<String>,
}

#[derive(Debug)]
enum Constructorlike {
    Constructor(Constructor),
    MacroDef(MacroDef),
    WithBlock(WithBlock),
}

#[derive(Debug)]
struct Constructor {
    id: Option<String>,
    display: String,
    p_equation: PEquation,
    context_block: ContextBlock,
    rtl_body: RtlBody,
}

#[derive(Debug)]
enum PEquation {
    EllEq(Box<EllEq>),
    And(Box<PEquation>, Box<PEquation>),
    Or(Box<PEquation>, Box<PEquation>),
    Cat(Box<PEquation>, Box<PEquation>),
}

#[derive(Debug)]
struct EllEq {
    ellipsis: bool,
    ell_rt: EllRt,
}

#[derive(Debug)]
struct EllRt {
    atomic: Atomic,
    ellipsis: bool,
}

#[derive(Debug)]
enum Atomic {
    Constraint(Constraint),
    Parenthesized(PEquation),
}

#[derive(Debug)]
enum Constraint {
    Equal(String, PExpression),
    NotEqual(String, PExpression),
    Less(String, PExpression),
    LessEqual(String, PExpression),
    Greater(String, PExpression),
    GreaterEqual(String, PExpression),
    Symbol(String),
}

#[derive(Debug)]
enum PExpression {
    ConstantValue(u64),
    Symbol(String),
    Parenthesized(Box<PExpression>),
    Add(Box<PExpression>, Box<PExpression>),
    Sub(Box<PExpression>, Box<PExpression>),
    Mult(Box<PExpression>, Box<PExpression>),
    LeftShift(Box<PExpression>, Box<PExpression>),
    RightShift(Box<PExpression>, Box<PExpression>),
    And(Box<PExpression>, Box<PExpression>),
    Or(Box<PExpression>, Box<PExpression>),
    Xor(Box<PExpression>, Box<PExpression>),
    Div(Box<PExpression>, Box<PExpression>),
    Minus(Box<PExpression>),
    Not(Box<PExpression>),
}

#[derive(Debug)]
struct ContextBlock {
    context_list: Vec<ContextListItem>,
}

#[derive(Debug)]
enum ContextListItem {
    Eq(String, PExpression),
    Set(String, String),
}

#[derive(Debug)]
enum RtlBody {
    StandaloneSection(Rtl),
    FinalNamedSection(RtlContinue, RtlMid),
    OpUnimpl,
}

#[derive(Debug)]
struct Rtl {
    mid: RtlMid,
    export: Option<RtlExport>,
}

#[derive(Debug)]
enum RtlExport {
    ExportVarNode(ExportVarNode),
    SizedStar(SizedStar, String),
}

#[derive(Debug)]
enum ExportVarNode {
    Symbol { name: String },
    Tpl { i1: u8, i2: u8 },
}

#[derive(Debug)]
struct RtlMid {
    items: Vec<RtlMidItem>,
}

#[derive(Debug)]
enum RtlMidItem {
    Statement(Statement),
    Local(String, Option<u8>),
}

#[derive(Debug)]
enum Statement {
    Assign(String, Option<u8>, Expr),
    LocalAssign(String, Option<u8>, Expr),
    Store(SizedStar, Expr, Expr),
    MiscCall(String, Vec<Expr>),
    AssignBitRange(String, u8, u8, Expr),
    Build(String),
    CrossBuild(String, String),
    DelaySlot(u8),
    GoTo(JumpDest),
    IfGoTo(Expr, JumpDest),
    GoToInd(Expr),
    Call(JumpDest),
    CallInd(Expr),
    Return(Expr),
    Label(Label),
}

#[derive(Debug)]
struct Label {
    name: String,
}

#[derive(Debug)]
enum JumpDest {
    Label(Label),
    Symbol(String),
}

#[derive(Debug)]
enum Expr {
    VarNode(VarNode),
    SizedStar(SizedStar, Box<Expr>),
    IntAdd(Box<Expr>, Box<Expr>),
    IntSub(Box<Expr>, Box<Expr>),
    IntEqual(Box<Expr>, Box<Expr>),
    IntNotEqual(Box<Expr>, Box<Expr>),
    IntLess(Box<Expr>, Box<Expr>),
    IntGreatEqual(Box<Expr>, Box<Expr>),
    IntLessEqual(Box<Expr>, Box<Expr>),
    IntGreat(Box<Expr>, Box<Expr>),
    IntSLess(Box<Expr>, Box<Expr>),
    IntSGreatEqual(Box<Expr>, Box<Expr>),
    IntSLessEqual(Box<Expr>, Box<Expr>),
    IntSGreat(Box<Expr>, Box<Expr>),
    IntTwoComp(Box<Expr>),
    IntNegate(Box<Expr>),
    IntXor(Box<Expr>, Box<Expr>),
    IntAnd(Box<Expr>, Box<Expr>),
    IntOr(Box<Expr>, Box<Expr>),
    IntLeft(Box<Expr>, Box<Expr>),
    IntRight(Box<Expr>, Box<Expr>),
    IntSRight(Box<Expr>, Box<Expr>),
    IntMult(Box<Expr>, Box<Expr>),
    IntDiv(Box<Expr>, Box<Expr>),
    IntSDiv(Box<Expr>, Box<Expr>),
    IntRem(Box<Expr>, Box<Expr>),
    IntSRem(Box<Expr>, Box<Expr>),
    BoolNegate(Box<Expr>),
    BoolXor(Box<Expr>, Box<Expr>),
    BoolAnd(Box<Expr>, Box<Expr>),
    BoolOr(Box<Expr>, Box<Expr>),
    FloatEqual(Box<Expr>, Box<Expr>),
    FloatNotEqual(Box<Expr>, Box<Expr>),
    FloatLess(Box<Expr>, Box<Expr>),
    FloatGreat(Box<Expr>, Box<Expr>),
    FloatLessEqual(Box<Expr>, Box<Expr>),
    FloatGreatEqual(Box<Expr>, Box<Expr>),
    FloatAdd(Box<Expr>, Box<Expr>),
    FloatSub(Box<Expr>, Box<Expr>),
    FloatMult(Box<Expr>, Box<Expr>),
    FloatDiv(Box<Expr>, Box<Expr>),
    FloatNeg(Box<Expr>),
    FloatAbs(Box<Expr>),
    FloatSqrt(Box<Expr>),
    IntSext(Box<Expr>),
    IntZext(Box<Expr>),
    IntCarry(Box<Expr>, Box<Expr>),
    IntSCarry(Box<Expr>, Box<Expr>),
    IntSBorrow(Box<Expr>, Box<Expr>),
    FloatFloatToFloat(Box<Expr>),
    FloatIntToFloat(Box<Expr>),
    FloatNan(Box<Expr>),
    FloatTrunc(Box<Expr>),
    FloatCeil(Box<Expr>),
    FloatFloor(Box<Expr>),
    FloatRound(Box<Expr>),
    New1(Box<Expr>),
    New2(Box<Expr>, Box<Expr>),
    PopCount(Box<Expr>),
    BitRange1(String, u8),
    BitRange2(String, u8, u8),
    UserOp { name: String, args: Vec<Expr> },
}

#[derive(Debug)]
enum VarNode {
    SpecificSymbol { name: String },
    IntegerVarNode(Box<IntegerVarNode>),
}

#[derive(Debug)]
enum IntegerVarNode {
    S1(u64),
    S2(u64, u64),
    Address(VarNode),
    Address1(u64, VarNode),
}

#[derive(Debug)]
enum SizedStar {
    S0,
    S1(u8),
    S2(String),
    S3(String, u8),
}

#[derive(Debug)]
struct RtlContinue {
    first_section: RtlFirstSection,
    items: Vec<(RtlMid, SectionDef)>,
}

#[derive(Debug)]
struct RtlFirstSection {
    rtl: Rtl,
    section_def: SectionDef,
}

#[derive(Debug)]
struct SectionDef(String);

#[derive(Debug)]
struct MacroDef {
    start: MacroStart,
    rtl: Rtl,
}

#[derive(Debug)]
struct MacroStart {
    name: String,
    params: Vec<String>,
}

#[derive(Debug)]
struct WithBlock {
    mid: WithBlockMid,
}

#[derive(Debug)]
struct WithBlockMid {
    start: WithBlockStart,
    items: Vec<DefinitionOrConstructorlike>,
}

#[derive(Debug)]
struct WithBlockStart {
    id: Option<String>,
    bitpat: Option<PEquation>,
    block: ContextBlock,
}

#[derive(Debug)]
enum DefinitionOrConstructorlike {
    Definition(Definition),
    Constructorlike(Constructorlike),
}

#[derive(Debug)]
enum PreProcessDirective<'a> {
    Include { file_path: &'a str },
    Define { var_name: &'a str, value: &'a str },
    Undef { var_name: &'a str },
    IfDef { condition: bool },
    IfNDef { condition: bool },
    If { condition: bool },
    ElIf { condition: bool },
    EndIf,
    Else,
    Unknown { line: &'a str },
}

fn identifier_ref(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        satisfy(|c| c == '_' || c == '.' || c.is_ascii_alphabetic()),
        take_while(|c: char| c == '_' || c == '.' || c.is_ascii_alphanumeric()),
    ))(input)
}

fn identifier(input: &str) -> IResult<&str, String> {
    map(ws(identifier_ref), ToOwned::to_owned)(input)
}

fn string_ref(input: &str) -> IResult<&str, &str> {
    let q = '"';
    delimited(char(q), take_till(move |c| c == q), char(q))(input)
}

fn preprocessor_value(input: &str) -> IResult<&str, &str> {
    alt((string_ref, digit1))(input)
}

fn string(input: &str) -> IResult<&str, String> {
    map(ws(string_ref), ToOwned::to_owned)(input)
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
        0,
        &OpTable::new(&[&[("||", |l, r| *l || *r)], &[("&&", |l, r| *l && *r)]]),
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

fn preprocess_internal(input: impl BufRead, out: &mut String, defs: &mut HashMap<String, String>) {
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
                    let f = File::open(file_path).unwrap();
                    let r = BufReader::new(f);
                    preprocess_internal(r, out, defs);
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

fn preprocess(input: impl BufRead) -> String {
    let mut ret = String::new();
    let mut defs = HashMap::new();
    preprocess_internal(input, &mut ret, &mut defs);
    ret
}

fn parse_sleigh(input: &str) -> IResult<&str, Sleigh> {
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

fn parse_token_def(input: &str) -> IResult<&str, TokenDef> {
    map(
        delimited(
            pair(tok("define"), tok("token")),
            tuple((
                identifier,
                delimited(char('('), ws(u8), char(')')),
                opt(preceded(pair(tok("endian"), tok("=")), parse_endian)),
                many1(parse_field_def),
            )),
            tok(";"),
        ),
        |(name, i, endian, fields)| TokenDef {
            name,
            i,
            endian,
            fields,
        },
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
        |(var_sym, fields)| ContextDef { var_sym, fields },
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
        |(value_list, int_b_list)| ValueAttach {
            value_list,
            int_b_list,
        },
    )(input)
}

fn parse_name_attach(input: &str) -> IResult<&str, NameAttach> {
    map(
        delimited(
            pair(tok("attach"), tok("names")),
            pair(identifier_list, any_string_list),
            tok(";"),
        ),
        |(value_list, string_list)| NameAttach {
            value_list,
            string_list,
        },
    )(input)
}

fn parse_var_attach(input: &str) -> IResult<&str, VarAttach> {
    map(
        delimited(
            pair(tok("attach"), tok("variables")),
            pair(identifier_list, identifier_list),
            tok(";"),
        ),
        |(value_list, string_list)| VarAttach {
            value_list,
            string_list,
        },
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

fn parse_constructor(input: &str) -> IResult<&str, Constructor> {
    map(
        tuple((
            terminated(opt(identifier), char(':')),
            map(
                terminated(ws(alt((take_until("\tis"), take_until(" is")))), tok("is")),
                ToOwned::to_owned,
            ),
            parse_p_equation,
            parse_context_block,
            parse_rtl_body,
        )),
        |(id, display, p_equation, context_block, rtl_body)| Constructor {
            id,
            display,
            p_equation,
            context_block,
            rtl_body,
        },
    )(input)
}

fn parse_p_expression_atom(input: &str) -> IResult<&str, PExpression> {
    alt((
        map(parse_u64, PExpression::ConstantValue),
        map(identifier, PExpression::Symbol),
        map(
            delimited(tok("("), |input| parse_p_expression(0, input), tok(")")),
            |e| PExpression::Parenthesized(Box::new(e)),
        ),
        map(preceded(tok("-"), parse_p_expression_atom), |e| {
            PExpression::Minus(Box::new(e))
        }),
        map(preceded(tok("~"), parse_p_expression_atom), |e| {
            PExpression::Not(Box::new(e))
        }),
    ))(input)
}

fn parse_p_expression(min_prec: usize, input: &str) -> IResult<&str, PExpression> {
    op_prec(
        &parse_p_expression_atom,
        min_prec,
        &OpTable::new(&[
            &[("$or", PExpression::Or), ("|", PExpression::Or)],
            &[("$xor", PExpression::Xor), ("^", PExpression::Xor)],
            &[("$and", PExpression::And), ("&", PExpression::And)],
            &[
                ("<<", PExpression::LeftShift),
                (">>", PExpression::RightShift),
            ],
            &[("+", PExpression::Add), ("-", PExpression::Sub)],
            &[("*", PExpression::Mult), ("/", PExpression::Div)],
        ]),
        input,
    )
}

fn parse_constraint(input: &str) -> IResult<&str, Constraint> {
    alt((
        map(
            separated_pair(identifier, tok("="), |input| parse_p_expression(3, input)),
            |(i, e)| Constraint::Equal(i, e),
        ),
        map(
            separated_pair(identifier, tok("!="), |input| parse_p_expression(3, input)),
            |(i, e)| Constraint::NotEqual(i, e),
        ),
        map(
            separated_pair(identifier, tok("<"), |input| parse_p_expression(3, input)),
            |(i, e)| Constraint::Less(i, e),
        ),
        map(
            separated_pair(identifier, tok("<="), |input| parse_p_expression(3, input)),
            |(i, e)| Constraint::LessEqual(i, e),
        ),
        map(
            separated_pair(identifier, tok(">"), |input| parse_p_expression(3, input)),
            |(i, e)| Constraint::Greater(i, e),
        ),
        map(
            separated_pair(identifier, tok(">="), |input| parse_p_expression(3, input)),
            |(i, e)| Constraint::GreaterEqual(i, e),
        ),
        map(identifier, Constraint::Symbol),
    ))(input)
}

fn parse_atomic(input: &str) -> IResult<&str, Atomic> {
    alt((
        map(parse_constraint, Atomic::Constraint),
        map(
            delimited(tok("("), parse_p_equation, tok(")")),
            Atomic::Parenthesized,
        ),
    ))(input)
}

fn parse_ell_rt(input: &str) -> IResult<&str, EllRt> {
    map(pair(parse_atomic, parse_ellipsis), |(atomic, ellipsis)| {
        EllRt { atomic, ellipsis }
    })(input)
}

fn parse_ell_eq(input: &str) -> IResult<&str, EllEq> {
    map(pair(parse_ellipsis, parse_ell_rt), |(ellipsis, ell_rt)| {
        EllEq { ellipsis, ell_rt }
    })(input)
}

fn parse_ellipsis(input: &str) -> IResult<&str, bool> {
    map(opt(tok("...")), |o| o.is_some())(input)
}

fn parse_p_equation_ell_eq(input: &str) -> IResult<&str, PEquation> {
    map(parse_ell_eq, |e| PEquation::EllEq(Box::new(e)))(input)
}

fn parse_p_equation(input: &str) -> IResult<&str, PEquation> {
    op_prec(
        &parse_p_equation_ell_eq,
        0,
        &OpTable::new(&[
            &[("|", PEquation::Or)],
            &[(";", PEquation::Cat)],
            &[("&", PEquation::And)],
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
        map(separated_pair(ws(u8), tok(":"), ws(u8)), |(i1, i2)| {
            ExportVarNode::Tpl { i1, i2 }
        }),
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
        0,
        &OpTable::new(&[
            &[("||", Expr::BoolOr)],
            &[("&&", Expr::BoolAnd), ("^^", Expr::BoolXor)],
            &[("|", Expr::IntOr)],
            &[("^", Expr::IntXor)],
            &[("&", Expr::IntAnd)],
            &[
                ("==", Expr::IntEqual),
                ("!=", Expr::IntNotEqual),
                ("f==", Expr::FloatEqual),
                ("f!=", Expr::FloatNotEqual),
            ],
            &[
                ("<", Expr::IntLess),
                (">", Expr::IntGreat),
                (">=", Expr::IntGreatEqual),
                ("<=", Expr::IntLessEqual),
                ("s<", Expr::IntSLess),
                ("s>=", Expr::IntSGreatEqual),
                ("s<=", Expr::IntSLessEqual),
                ("s>", Expr::IntSGreat),
                ("f<", Expr::FloatLess),
                ("f>", Expr::FloatGreat),
                ("f<=", Expr::FloatLessEqual),
                ("f>=", Expr::FloatGreatEqual),
            ],
            &[
                ("<<", Expr::IntLeft),
                (">>", Expr::IntRight),
                ("s>>", Expr::IntSRight),
            ],
            &[
                ("+", Expr::IntAdd),
                ("-", Expr::IntSub),
                ("f+", Expr::FloatAdd),
                ("f-", Expr::FloatSub),
            ],
            &[
                ("*", Expr::IntMult),
                ("/", Expr::IntDiv),
                ("%", Expr::IntRem),
                ("s/", Expr::IntSDiv),
                ("s%", Expr::IntSRem),
                ("f*", Expr::FloatMult),
                ("f/", Expr::FloatDiv),
            ],
        ]),
        input,
    )
}

fn parse_expr_func_helper_1<'a>(
    func_name: &'static str,
    constructor: impl Fn(Box<Expr>) -> Expr,
    input: &'a str,
) -> IResult<&'a str, Expr> {
    map(
        preceded(tok(func_name), delimited(tok("("), parse_expr, tok(")"))),
        |e| constructor(Box::new(e)),
    )(input)
}

fn parse_expr_func_helper_2<'a>(
    func_name: &'static str,
    constructor: impl Fn(Box<Expr>, Box<Expr>) -> Expr,
    input: &'a str,
) -> IResult<&'a str, Expr> {
    map(
        preceded(
            tok(func_name),
            delimited(
                tok("("),
                separated_pair(parse_expr, tok(","), parse_expr),
                tok(")"),
            ),
        ),
        |(e1, e2)| constructor(Box::new(e1), Box::new(e2)),
    )(input)
}

fn parse_expr_func(input: &str) -> IResult<&str, Expr> {
    alt((
        |input| parse_expr_func_helper_1("abs", Expr::FloatAbs, input),
        |input| parse_expr_func_helper_1("sqrt", Expr::FloatSqrt, input),
        |input| parse_expr_func_helper_1("sext", Expr::IntSext, input),
        |input| parse_expr_func_helper_1("zext", Expr::IntZext, input),
        |input| parse_expr_func_helper_2("carry", Expr::IntCarry, input),
        |input| parse_expr_func_helper_2("scarry", Expr::IntSCarry, input),
        |input| parse_expr_func_helper_2("sborrow", Expr::IntSBorrow, input),
        |input| parse_expr_func_helper_1("float2float", Expr::FloatFloatToFloat, input),
        |input| parse_expr_func_helper_1("int2float", Expr::FloatIntToFloat, input),
        |input| parse_expr_func_helper_1("nan", Expr::FloatNan, input),
        |input| parse_expr_func_helper_1("trunc", Expr::FloatTrunc, input),
        |input| parse_expr_func_helper_1("ceil", Expr::FloatCeil, input),
        |input| parse_expr_func_helper_1("floor", Expr::FloatFloor, input),
        |input| parse_expr_func_helper_1("round", Expr::FloatRound, input),
        |input| parse_expr_func_helper_1("new", Expr::New1, input),
        |input| parse_expr_func_helper_2("new", Expr::New2, input),
        |input| parse_expr_func_helper_1("popcount", Expr::PopCount, input),
    ))(input)
}

fn parse_expr_atom(input: &str) -> IResult<&str, Expr> {
    alt((
        delimited(tok("("), parse_expr, tok(")")),
        map(pair(parse_sized_star, parse_expr_atom), |(ss, e)| {
            Expr::SizedStar(ss, Box::new(e))
        }),
        map(preceded(tok("-"), parse_expr_atom), |e| {
            Expr::IntTwoComp(Box::new(e))
        }),
        map(preceded(tok("~"), parse_expr_atom), |e| {
            Expr::IntNegate(Box::new(e))
        }),
        map(preceded(tok("!"), parse_expr_atom), |e| {
            Expr::BoolNegate(Box::new(e))
        }),
        map(preceded(tok("f-"), parse_expr_atom), |e| {
            Expr::FloatNeg(Box::new(e))
        }),
        parse_expr_func,
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
                opt(parse_p_equation),
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

struct OpTable<E: 'static> {
    op_precs: HashMap<&'static str, usize>,
    op_fns: HashMap<&'static str, fn(Box<E>, Box<E>) -> E>,
    op_tags: HashSet<&'static str>,
}

impl<E> OpTable<E> {
    fn new(bin_ops: &'static [&'static [(&'static str, fn(Box<E>, Box<E>) -> E)]]) -> Self {
        Self {
            op_precs: bin_ops
                .iter()
                .enumerate()
                .flat_map(|(prec, i)| i.iter().map(move |i| (prec, i)))
                .map(|(prec, (tag, _))| (*tag, prec))
                .collect(),
            op_fns: bin_ops.iter().flat_map(|i| i.iter()).cloned().collect(),
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

fn op_prec<'a, F, E>(
    e: &F,
    min_prec: usize,
    ops: &OpTable<E>,
    orig_input: &'a str,
) -> IResult<&'a str, E>
where
    F: Fn(&'a str) -> IResult<&'a str, E, nom::error::Error<&'a str>>,
{
    fn peek_op<'a, E>(input: &'a str, ops: &OpTable<E>) -> Option<&'static str> {
        let t = ops
            .op_tags
            .iter()
            .filter_map(|t| peek(tok(t))(input).ok())
            .map(|o| o.1)
            .max_by_key(|t| t.len())?;
        ops.lookup(t)
    }

    fn op_prec_1<'a, F, E>(
        mut lhs: E,
        min_prec: usize,
        ops: &OpTable<E>,
        e: &F,
        input: &mut &'a str,
    ) -> IResult<&'a str, E>
    where
        F: Fn(&'a str) -> IResult<&'a str, E, nom::error::Error<&'a str>>,
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
                (*input, rhs) = op_prec_1(rhs, op_prec + 1, ops, e, input)?;
                lookahead = peek_op(*input, ops);
            }
            lhs = ops.op_fns.get(op).unwrap()(Box::new(lhs), Box::new(rhs));
        }

        Ok((*input, lhs))
    }

    let (mut input, lhs) = e(orig_input)?;
    op_prec_1(lhs, min_prec, ops, e, &mut input)
}

fn main() {
    let pp = preprocess(std::io::stdin().lock());
    let x = parse_sleigh(&pp);
    println!("{x:?}");
}
