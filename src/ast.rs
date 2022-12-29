pub const INSTRUCTION: &str = "instruction";

#[derive(Debug, Clone)]
pub struct Sleigh {
    pub defs: Vec<Def>,
}

#[derive(Debug, Clone)]
pub enum Def {
    EndianDef(EndianDef),
    AlignDef(AlignDef),
    Definition(Definition),
    Constructorlike(Constructorlike),
}

#[derive(Debug, Clone)]
pub struct EndianDef {
    pub endian: Endian,
}

#[derive(Debug, Copy, Clone)]
pub enum Endian {
    Big,
    Little,
}

#[derive(Debug, Clone)]
pub struct AlignDef {
    pub align: u8,
}

#[derive(Debug, Clone)]
pub enum Definition {
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
pub struct TokenDef {
    pub name: String,
    pub size: u8,
    pub endian: Option<Endian>,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: String,
    pub low: u8,
    pub high: u8,
    pub signed: bool,
    pub base: Option<Base>,
}

#[derive(Debug, Clone)]
pub enum Base {
    Hex,
    Dec,
}

#[derive(Debug, Clone)]
pub struct ContextDef {
    pub var_sym: String,
    pub fields: Vec<ContextFieldDef>,
}

#[derive(Debug, Clone)]
pub struct ContextFieldDef {
    pub name: String,
    pub low: u8,
    pub high: u8,
    pub signed: bool,
    pub noflow: bool,
    pub base: Option<Base>,
}

#[derive(Debug, Clone)]
pub struct SpaceDef {
    pub name: String,
    pub typ: SpaceType,
    pub size: u8,
    pub default: bool,
}

#[derive(Debug, Clone)]
pub enum SpaceType {
    Ram,
    Register,
}

#[derive(Debug, Clone)]
pub struct VarNodeDef {
    pub space_sym: String,
    pub offset: u64,
    pub size: u64,
    pub names: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct BitRangeDef {
    pub bit_ranges: Vec<BitRangeSingle>,
}

#[derive(Debug, Clone)]
pub struct BitRangeSingle {
    pub name: String,
    pub var_sym: String,
    pub i1: u8,
    pub i2: u8,
}

#[derive(Debug, Clone)]
pub struct PCodeOpDef {
    pub ops: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ValueAttach {
    pub value_list: Vec<String>,
    pub int_b_list: Vec<i8>,
}

#[derive(Debug, Clone)]
pub struct NameAttach {
    pub value_list: Vec<String>,
    pub string_list: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct VarAttach {
    pub value_list: Vec<String>,
    pub string_list: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Constructorlike {
    Constructor(Constructor<()>),
    MacroDef(MacroDef),
    WithBlock(WithBlock),
}

#[derive(Debug, Clone)]
pub enum DisplayToken {
    Caret,
    String(String),
    Char(char),
    Space,
    Symbol(String),
}

#[derive(Debug, Clone)]
pub struct DisplaySection {
    pub toks: Vec<DisplayToken>,
}

#[derive(Debug, Clone)]
pub struct Constructor<T> {
    pub header: String,
    pub display: DisplaySection,
    pub p_equation: PatternEquation<T>,
    pub context_block: ContextBlock,
    pub rtl_body: RtlBody,
}

#[derive(Debug, Clone)]
pub enum PatternEquationBinOp {
    And,
    Or,
    Cat,
}

#[derive(Debug, Clone)]
pub struct PatternEquationBin<T> {
    pub op: PatternEquationBinOp,
    pub l: PatternEquation<T>,
    pub r: PatternEquation<T>,
}

#[derive(Debug, Clone)]
pub enum PatternEquationInner<T> {
    EllEq(Box<EllEq<T>>),
    Bin(Box<PatternEquationBin<T>>),
}

#[derive(Debug, Clone)]
pub struct PatternEquation<T> {
    pub inner: PatternEquationInner<T>,
    pub token: T,
}

#[derive(Debug, Clone)]
pub struct EllEq<T> {
    pub ellipsis_left: bool,
    pub ell_rt: EllRt<T>,
}

#[derive(Debug, Clone)]
pub struct EllRt<T> {
    pub atomic: Atomic<T>,
    pub ellipsis_right: bool,
}

#[derive(Debug, Clone)]
pub enum Atomic<T> {
    Constraint(Constraint),
    Parenthesized(PatternEquation<T>),
}

#[derive(Debug, Clone)]
pub enum ConstraintCompareOp {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub struct ConstraintCompare {
    pub op: ConstraintCompareOp,
    pub symbol: String,
    pub expr: PExpression,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Compare(ConstraintCompare),
    Symbol(String),
}

#[derive(Debug, Clone)]
pub enum PExpressionBinOp {
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
pub struct PExpressionBin {
    pub op: PExpressionBinOp,
    pub l: PExpression,
    pub r: PExpression,
}

#[derive(Debug, Clone)]
pub enum PExpressionUnaryOp {
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub struct PExpressionUnary {
    pub op: PExpressionUnaryOp,
    pub operand: PExpression,
}

#[derive(Debug, Clone)]
pub enum PExpression {
    ConstantValue(u64),
    Symbol(String),
    Bin(Box<PExpressionBin>),
    Unary(Box<PExpressionUnary>),
}

#[derive(Debug, Clone)]
pub struct ContextBlock {
    pub context_list: Vec<ContextListItem>,
}

#[derive(Debug, Clone)]
pub enum ContextListItem {
    Eq(String, PExpression),
    Set(String, String),
}

#[derive(Debug, Clone)]
pub enum RtlBody {
    StandaloneSection(Rtl),
    FinalNamedSection(RtlContinue, RtlMid),
    OpUnimpl,
}

#[derive(Debug, Clone)]
pub struct Rtl {
    pub mid: RtlMid,
    pub export: Option<RtlExport>,
}

#[derive(Debug, Clone)]
pub enum RtlExport {
    ExportVarNode(ExportVarNode),
    SizedStar(SizedStar, String),
}

#[derive(Debug, Clone)]
pub enum ExportVarNode {
    Symbol { name: String },
    Tpl { i1: u8, i2: u8 },
}

#[derive(Debug, Clone)]
pub struct RtlMid {
    pub items: Vec<RtlMidItem>,
}

#[derive(Debug, Clone)]
pub enum RtlMidItem {
    Statement(Statement),
    Local(String, Option<u8>),
}

#[derive(Debug, Clone)]
pub enum Statement {
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
pub struct Label {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum JumpDest {
    Label(Label),
    Symbol(String),
}

#[derive(Debug, Clone)]
pub enum ExprBinOp {
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
pub struct ExprBin {
    pub op: ExprBinOp,
    pub l: Expr,
    pub r: Expr,
}

#[derive(Debug, Clone)]
pub enum ExprUnaryOp {
    IntTwoComp,
    IntNegate,
    BoolNegate,
    FloatNeg,
}

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: ExprUnaryOp,
    pub operand: Expr,
}

#[derive(Debug, Clone)]
pub enum ExprFunc1Name {
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
pub enum ExprFunc2Name {
    IntCarry,
    IntSCarry,
    IntSBorrow,
    New,
}

#[derive(Debug, Clone)]
pub struct ExprFunc1 {
    pub name: ExprFunc1Name,
    pub arg: Expr,
}

#[derive(Debug, Clone)]
pub struct ExprFunc2 {
    pub name: ExprFunc2Name,
    pub arg1: Expr,
    pub arg2: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
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
pub enum VarNode {
    SpecificSymbol { name: String },
    IntegerVarNode(Box<IntegerVarNode>),
}

#[derive(Debug, Clone)]
pub enum IntegerVarNode {
    S1(u64),
    S2(u64, u64),
    Address(VarNode),
    Address1(u64, VarNode),
}

#[derive(Debug, Clone)]
pub enum SizedStar {
    S0,
    S1(u8),
    S2(String),
    S3(String, u8),
}

#[derive(Debug, Clone)]
pub struct RtlContinue {
    pub first_section: RtlFirstSection,
    pub items: Vec<(RtlMid, SectionDef)>,
}

#[derive(Debug, Clone)]
pub struct RtlFirstSection {
    pub rtl: Rtl,
    pub section_def: SectionDef,
}

#[derive(Debug, Clone)]
pub struct SectionDef(String);

#[derive(Debug, Clone)]
pub struct MacroDef {
    pub start: MacroStart,
    pub rtl: Rtl,
}

#[derive(Debug, Clone)]
pub struct MacroStart {
    pub name: String,
    pub params: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct WithBlock {
    pub mid: WithBlockMid,
}

#[derive(Debug, Clone)]
pub struct WithBlockMid {
    pub start: WithBlockStart,
    pub items: Vec<DefinitionOrConstructorlike>,
}

#[derive(Debug, Clone)]
pub struct WithBlockStart {
    pub id: Option<String>,
    pub bitpat: Option<PatternEquation<()>>,
    pub block: ContextBlock,
}

#[derive(Debug, Clone)]
pub enum DefinitionOrConstructorlike {
    Definition(Definition),
    Constructorlike(Constructorlike),
}

#[derive(Debug, Clone)]
pub enum PreProcessDirective<'a> {
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

impl std::fmt::Display for DisplayToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DisplayToken::Caret => write!(f, "^"),
            DisplayToken::String(s) => write!(f, "\"{s}\""),
            DisplayToken::Char(c) => write!(f, "{c}"),
            DisplayToken::Space => write!(f, " "),
            DisplayToken::Symbol(s) => write!(f, "{s}"),
        }
    }
}

impl std::fmt::Display for DisplaySection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for tok in &self.toks {
            write!(f, "{tok}")?;
        }
        Ok(())
    }
}
