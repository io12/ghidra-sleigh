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
    combinator::{fail, map, map_opt, map_res, opt, peek, recognize, success, value},
    multi::{many0, many1, many_till, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use regex::Regex;

#[derive(Debug, Clone)]
struct Sleigh {
    defs: Vec<Def>,
}

#[derive(Debug, Clone)]
enum Def {
    EndianDef(EndianDef),
    AlignDef(AlignDef),
    Definition(Definition),
    Constructorlike(Constructorlike),
}

#[derive(Debug, Clone)]
struct EndianDef {
    endian: Endian,
}

#[derive(Debug, Clone)]
enum Endian {
    Big,
    Little,
}

#[derive(Debug, Clone)]
struct AlignDef {
    align: u8,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct TokenDef {
    name: String,
    size: u8,
    endian: Option<Endian>,
    fields: Vec<FieldDef>,
}

#[derive(Debug, Clone)]
struct FieldDef {
    name: String,
    low: u8,
    high: u8,
    signed: bool,
    base: Option<Base>,
}

#[derive(Debug, Clone)]
enum Base {
    Hex,
    Dec,
}

#[derive(Debug, Clone)]
struct ContextDef {
    var_sym: String,
    fields: Vec<ContextFieldDef>,
}

#[derive(Debug, Clone)]
struct ContextFieldDef {
    name: String,
    low: u8,
    high: u8,
    signed: bool,
    noflow: bool,
    base: Option<Base>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct VarNodeDef {
    space_sym: String,
    offset: u64,
    size: u64,
    names: Vec<String>,
}

#[derive(Debug, Clone)]
struct BitRangeDef {
    bit_ranges: Vec<BitRangeSingle>,
}

#[derive(Debug, Clone)]
struct BitRangeSingle {
    name: String,
    var_sym: String,
    i1: u8,
    i2: u8,
}

#[derive(Debug, Clone)]
struct PCodeOpDef {
    ops: Vec<String>,
}

#[derive(Debug, Clone)]
struct ValueAttach {
    value_list: Vec<String>,
    int_b_list: Vec<i8>,
}

#[derive(Debug, Clone)]
struct NameAttach {
    value_list: Vec<String>,
    string_list: Vec<String>,
}

#[derive(Debug, Clone)]
struct VarAttach {
    value_list: Vec<String>,
    string_list: Vec<String>,
}

#[derive(Debug, Clone)]
enum Constructorlike {
    Constructor(Constructor),
    MacroDef(MacroDef),
    WithBlock(WithBlock),
}

#[derive(Debug, Clone)]
enum DisplayToken {
    Caret,
    String(String),
    Char(char),
    Space,
    Symbol(String),
}

#[derive(Debug, Clone)]
struct DisplaySection {
    toks: Vec<DisplayToken>,
}

#[derive(Debug, Clone)]
struct Constructor {
    id: Option<String>,
    display: DisplaySection,
    p_equation: PEquation,
    context_block: ContextBlock,
    rtl_body: RtlBody,
}

#[derive(Debug, Clone)]
enum PEquation {
    EllEq(Box<EllEq>),
    And(Box<PEquation>, Box<PEquation>),
    Or(Box<PEquation>, Box<PEquation>),
    Cat(Box<PEquation>, Box<PEquation>),
}

#[derive(Debug, Clone)]
struct EllEq {
    ellipsis: bool,
    ell_rt: EllRt,
}

#[derive(Debug, Clone)]
struct EllRt {
    atomic: Atomic,
    ellipsis: bool,
}

#[derive(Debug, Clone)]
enum Atomic {
    Constraint(Constraint),
    Parenthesized(PEquation),
}

#[derive(Debug, Clone)]
enum ConstraintCompareOp {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone)]
struct ConstraintCompare {
    op: ConstraintCompareOp,
    symbol: String,
    expr: PExpression,
}

#[derive(Debug, Clone)]
enum Constraint {
    Compare(ConstraintCompare),
    Symbol(String),
}

#[derive(Debug, Clone)]
enum PExpressionBinOp {
    Add,
    Sub,
    Mult,
    LeftShift,
    RightShift,
    And,
    Or,
    Xor,
    Div,
}

#[derive(Debug, Clone)]
struct PExpressionBin {
    op: PExpressionBinOp,
    l: PExpression,
    r: PExpression,
}

#[derive(Debug, Clone)]
enum PExpressionUnaryOp {
    Minus,
    Not,
}

#[derive(Debug, Clone)]
struct PExpressionUnary {
    op: PExpressionUnaryOp,
    operand: PExpression,
}

#[derive(Debug, Clone)]
enum PExpression {
    ConstantValue(u64),
    Symbol(String),
    Bin(Box<PExpressionBin>),
    Unary(Box<PExpressionUnary>),
}

#[derive(Debug, Clone)]
struct ContextBlock {
    context_list: Vec<ContextListItem>,
}

#[derive(Debug, Clone)]
enum ContextListItem {
    Eq(String, PExpression),
    Set(String, String),
}

#[derive(Debug, Clone)]
enum RtlBody {
    StandaloneSection(Rtl),
    FinalNamedSection(RtlContinue, RtlMid),
    OpUnimpl,
}

#[derive(Debug, Clone)]
struct Rtl {
    mid: RtlMid,
    export: Option<RtlExport>,
}

#[derive(Debug, Clone)]
enum RtlExport {
    ExportVarNode(ExportVarNode),
    SizedStar(SizedStar, String),
}

#[derive(Debug, Clone)]
enum ExportVarNode {
    Symbol { name: String },
    Tpl { i1: u8, i2: u8 },
}

#[derive(Debug, Clone)]
struct RtlMid {
    items: Vec<RtlMidItem>,
}

#[derive(Debug, Clone)]
enum RtlMidItem {
    Statement(Statement),
    Local(String, Option<u8>),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct Label {
    name: String,
}

#[derive(Debug, Clone)]
enum JumpDest {
    Label(Label),
    Symbol(String),
}

#[derive(Debug, Clone)]
enum ExprBinOp {
    IntAdd,
    IntSub,
    IntEqual,
    IntNotEqual,
    IntLess,
    IntGreatEqual,
    IntLessEqual,
    IntGreat,
    IntSLess,
    IntSGreatEqual,
    IntSLessEqual,
    IntSGreat,
    IntXor,
    IntAnd,
    IntOr,
    IntLeft,
    IntRight,
    IntSRight,
    IntMult,
    IntDiv,
    IntSDiv,
    IntRem,
    IntSRem,
    BoolXor,
    BoolAnd,
    BoolOr,
    FloatEqual,
    FloatNotEqual,
    FloatLess,
    FloatGreat,
    FloatLessEqual,
    FloatGreatEqual,
    FloatAdd,
    FloatSub,
    FloatMult,
    FloatDiv,
}

#[derive(Debug, Clone)]
struct ExprBin {
    op: ExprBinOp,
    l: Expr,
    r: Expr,
}

#[derive(Debug, Clone)]
enum ExprUnaryOp {
    IntTwoComp,
    IntNegate,
    BoolNegate,
    FloatNeg,
}

#[derive(Debug, Clone)]
struct ExprUnary {
    op: ExprUnaryOp,
    operand: Expr,
}

#[derive(Debug, Clone)]
enum ExprFunc1Name {
    FloatAbs,
    FloatSqrt,
    IntSext,
    IntZext,
    FloatFloatToFloat,
    FloatIntToFloat,
    FloatNan,
    FloatTrunc,
    FloatCeil,
    FloatFloor,
    FloatRound,
    New,
    PopCount,
}

#[derive(Debug, Clone)]
enum ExprFunc2Name {
    IntCarry,
    IntSCarry,
    IntSBorrow,
    New,
}

#[derive(Debug, Clone)]
struct ExprFunc1 {
    name: ExprFunc1Name,
    arg: Expr,
}

#[derive(Debug, Clone)]
struct ExprFunc2 {
    name: ExprFunc2Name,
    arg1: Expr,
    arg2: Expr,
}

#[derive(Debug, Clone)]
enum Expr {
    VarNode(VarNode),
    SizedStar(SizedStar, Box<Expr>),
    Bin(Box<ExprBin>),
    Unary(Box<ExprUnary>),
    BitRange1(String, u8),
    BitRange2(String, u8, u8),
    Func1(Box<ExprFunc1>),
    Func2(Box<ExprFunc2>),
    UserOp { name: String, args: Vec<Expr> },
}

#[derive(Debug, Clone)]
enum VarNode {
    SpecificSymbol { name: String },
    IntegerVarNode(Box<IntegerVarNode>),
}

#[derive(Debug, Clone)]
enum IntegerVarNode {
    S1(u64),
    S2(u64, u64),
    Address(VarNode),
    Address1(u64, VarNode),
}

#[derive(Debug, Clone)]
enum SizedStar {
    S0,
    S1(u8),
    S2(String),
    S3(String, u8),
}

#[derive(Debug, Clone)]
struct RtlContinue {
    first_section: RtlFirstSection,
    items: Vec<(RtlMid, SectionDef)>,
}

#[derive(Debug, Clone)]
struct RtlFirstSection {
    rtl: Rtl,
    section_def: SectionDef,
}

#[derive(Debug, Clone)]
struct SectionDef(String);

#[derive(Debug, Clone)]
struct MacroDef {
    start: MacroStart,
    rtl: Rtl,
}

#[derive(Debug, Clone)]
struct MacroStart {
    name: String,
    params: Vec<String>,
}

#[derive(Debug, Clone)]
struct WithBlock {
    mid: WithBlockMid,
}

#[derive(Debug, Clone)]
struct WithBlockMid {
    start: WithBlockStart,
    items: Vec<DefinitionOrConstructorlike>,
}

#[derive(Debug, Clone)]
struct WithBlockStart {
    id: Option<String>,
    bitpat: Option<PEquation>,
    block: ContextBlock,
}

#[derive(Debug, Clone)]
enum DefinitionOrConstructorlike {
    Definition(Definition),
    Constructorlike(Constructorlike),
}

#[derive(Debug, Clone)]
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
                map_opt(delimited(char('('), ws(u8), char(')')), |size| {
                    if size % 8 == 0 {
                        Some(size / 8)
                    } else {
                        None
                    }
                }),
                opt(preceded(pair(tok("endian"), tok("=")), parse_endian)),
                many1(parse_field_def),
            )),
            tok(";"),
        ),
        |(name, size, endian, fields)| TokenDef {
            name,
            size,
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
    map(
        ws(many_till(parse_display_token, tok("is"))),
        |(toks, _)| DisplaySection { toks },
    )(input)
}

fn parse_constructor(input: &str) -> IResult<&str, Constructor> {
    map(
        tuple((
            terminated(opt(identifier), char(':')),
            parse_display_section,
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
        &|op: fn(Box<PEquation>, Box<PEquation>) -> PEquation, l, r| op(Box::new(l), Box::new(r)),
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

struct OffAndSize {
    off: u64,
    size: u64,
}

struct TokenField {
    token_size: u8,
    low: u8,
    high: u8,
}

struct SleighContext {
    endian: Endian,
    align: u8,
    tokens: HashMap<String, TokenField>,
    contexts: HashMap<String, ContextDef>,
    spaces: HashMap<String, SpaceDef>,
    default_space: SpaceDef,
    var_nodes: HashMap<String, OffAndSize>,
    p_code_ops: HashSet<String>,
    constructors: Vec<Constructor>,
}

fn make_context(sleigh: &Sleigh) -> SleighContext {
    let mut endian = None;
    let mut align = None;
    let mut tokens = HashMap::new();
    let mut contexts = HashMap::new();
    let mut spaces = HashMap::new();
    let mut default_space = None;
    let mut var_nodes = HashMap::new();
    let mut p_code_ops = HashSet::new();
    let mut constructors = Vec::new();
    for def in &sleigh.defs {
        match def {
            Def::EndianDef(endian_def) => endian = Some(endian_def.endian.clone()),
            Def::AlignDef(align_def) => align = Some(align_def.align),
            Def::Definition(def) => match def {
                Definition::TokenDef(TokenDef {
                    name: _,
                    size,
                    endian: _,
                    fields,
                }) => {
                    for field in fields {
                        tokens.insert(
                            field.name.clone(),
                            TokenField {
                                token_size: *size,
                                low: field.low,
                                high: field.high,
                            },
                        );
                    }
                }
                Definition::ContextDef(context_def) => {
                    contexts.insert(context_def.var_sym.clone(), context_def.clone());
                }
                Definition::SpaceDef(space_def) => {
                    spaces.insert(space_def.name.clone(), space_def.clone());
                    if space_def.default {
                        default_space = Some(space_def.clone())
                    }
                }
                Definition::VarNodeDef(var_node_def) => {
                    for (i, name) in var_node_def.names.iter().enumerate() {
                        if name != "_" {
                            var_nodes.insert(
                                name.clone(),
                                OffAndSize {
                                    off: var_node_def.offset + i as u64 * var_node_def.size,
                                    size: var_node_def.size,
                                },
                            );
                        }
                    }
                }
                Definition::BitRangeDef(_) => todo!(),
                Definition::PCodeOpDef(p_code_op_def) => {
                    for op in &p_code_op_def.ops {
                        p_code_ops.insert(op.clone());
                    }
                }
                Definition::ValueAttach(_) => todo!(),
                Definition::NameAttach(_) => todo!(),
                Definition::VarAttach(_) => todo!(),
            },
            Def::Constructorlike(constructorlike) => match constructorlike {
                Constructorlike::Constructor(constructor) => {
                    constructors.push(constructor.clone());
                }
                Constructorlike::MacroDef(_) => {}
                Constructorlike::WithBlock(_) => todo!(),
            },
        }
    }

    SleighContext {
        endian: endian.unwrap(),
        align: align.unwrap(),
        tokens,
        contexts,
        spaces,
        default_space: default_space.unwrap(),
        var_nodes,
        p_code_ops,
        constructors,
    }
}

fn compute_token_symbol(ctx: &SleighContext, symbol: &str, input: &[u8]) -> u8 {
    let TokenField {
        token_size: _,
        low,
        high,
    } = ctx.tokens.get(symbol).unwrap();
    let low = *low as u64;
    let high = *high as u64;
    let mask = (((1 << (high + 1)) - 1) - ((1 << low) - 1)) as u8;
    assert!(high <= 7);
    (input.get(0).unwrap() & mask) >> low
}

fn compute_p_expression(ctx: &SleighContext, expr: &PExpression) -> u64 {
    match expr {
        PExpression::ConstantValue(v) => *v,
        PExpression::Symbol(_) => todo!(),
        PExpression::Bin(binary_expr) => {
            let PExpressionBin { op, l, r } = &**binary_expr;
            let l = compute_p_expression(ctx, l);
            let r = compute_p_expression(ctx, r);
            match op {
                PExpressionBinOp::Add => l + r,
                PExpressionBinOp::Sub => l - r,
                PExpressionBinOp::Mult => l * r,
                PExpressionBinOp::LeftShift => l << r,
                PExpressionBinOp::RightShift => l >> r,
                PExpressionBinOp::And => l & r,
                PExpressionBinOp::Or => l | r,
                PExpressionBinOp::Xor => l ^ r,
                PExpressionBinOp::Div => l / r,
            }
        }
        PExpression::Unary(unary_expr) => {
            let PExpressionUnary { op, operand } = &**unary_expr;
            let operand = compute_p_expression(ctx, operand);
            match op {
                PExpressionUnaryOp::Minus => -(operand as i64) as u64,
                PExpressionUnaryOp::Not => !operand,
            }
        }
    }
}

fn check_p_equation(ctx: &SleighContext, p_eq: &PEquation, input: &[u8]) -> bool {
    match p_eq {
        PEquation::EllEq(ell_eq) => {
            let EllEq {
                ellipsis: _,
                ell_rt:
                    EllRt {
                        atomic,
                        ellipsis: _,
                    },
            } = &**ell_eq;
            match atomic {
                Atomic::Constraint(constraint) => match constraint {
                    Constraint::Compare(ConstraintCompare { op, symbol, expr }) => {
                        let l = compute_token_symbol(ctx, &symbol, input);
                        let r = compute_p_expression(ctx, &expr);
                        let l = l as u64;
                        match op {
                            ConstraintCompareOp::Equal => l == r,
                            ConstraintCompareOp::NotEqual => l != r,
                            ConstraintCompareOp::Less => l < r,
                            ConstraintCompareOp::LessEqual => l <= r,
                            ConstraintCompareOp::Greater => l > r,
                            ConstraintCompareOp::GreaterEqual => l >= r,
                        }
                    }
                    Constraint::Symbol(_) => true,
                },
                Atomic::Parenthesized(p_eq) => check_p_equation(ctx, &p_eq, input),
            }
        }
        PEquation::And(l, r) | PEquation::Cat(l, r) => {
            let l = check_p_equation(ctx, &*l, input);
            let r = check_p_equation(ctx, &*r, input);
            l && r
        }
        PEquation::Or(l, r) => {
            let l = check_p_equation(ctx, &*l, input);
            let r = check_p_equation(ctx, &*r, input);
            l || r
        }
    }
}

// fn p_equation_symm_off(p_eq: &PEquation, target_symbol: &str) -> (OffAndSize, bool) {
//     match p_eq {
//         PEquation::EllEq(ell_eq) => {
//             let EllEq {
//                 ellipsis: _,
//                 ell_rt:
//                     EllRt {
//                         atomic,
//                         ellipsis: _,
//                     },
//             } = &**ell_eq;
//             match atomic {
//                 Atomic::Constraint(constraint) => match constraint {
//                     Constraint::Symbol(symbol) if symbol == target_symbol => Some(0),
//                     _ => None,
//                 },
//                 Atomic::Parenthesized(p_eq) => p_equation_symm_off(p_eq, target_symbol),
//             }
//         }
//         PEquation::And(l, r) | PEquation::Or(l, r) => {
//             let l = p_equation_symm_off(l, target_symbol);
//             let r = p_equation_symm_off(r, target_symbol);
//             l.or(r)
//         }
//         PEquation::Cat(l, r) => {
//             let l = p_equation_symm_off(l, target_symbol);
//             let r = p_equation_symm_off(r, target_symbol);
//         }
//     }
// }

fn disasm_insn(ctx: &SleighContext, table_id: Option<&str>, input: &[u8], out: &mut String) {
    dbg!(table_id, compute_token_symbol(ctx, "bbb", input));
    let constructor = &ctx
        .constructors
        .iter()
        .find(|c| c.id.as_deref() == table_id && check_p_equation(ctx, &c.p_equation, input))
        .unwrap();
    for tok in &constructor.display.toks {
        match tok {
            DisplayToken::Caret => {}
            DisplayToken::String(s) => out.push_str(s),
            DisplayToken::Char(c) => out.push(*c),
            DisplayToken::Space => out.push(' '),
            DisplayToken::Symbol(s) => {
                todo!()
                // if p_equation_symm_off(&constructor.p_equation, s) {
                //     disasm_insn(ctx, Some(s), input, out)
                // }
            }
        }
    }
}

fn main() {
    let pp = preprocess(std::io::stdin().lock());
    let (remaining, sleigh) = parse_sleigh(&pp).unwrap();
    assert!(remaining.is_empty());
    let ctx = make_context(&sleigh);
    let mut dis = String::new();
    disasm_insn(&ctx, None, &[0x96], &mut dis);
    println!("{dis}");
}
