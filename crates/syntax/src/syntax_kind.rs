use crate::token_kind::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // --- marker ---
    Error,

    // --- syntax ---
    SourceFile,
    StatementList,
    Include,
    Class,
    Def,
    Let,
    LetList,
    LetItem,
    MultiClass,
    Defm,
    Defset,
    Defvar,
    Foreach,
    ForeachIterator,
    If,
    Assert,
    TemplateArgList,
    TemplateArgDecl,
    RecordBody,
    ParentClassList,
    ClassRef,
    ArgValueList,
    PositionalArgValueList,
    NamedArgValueList,
    NamedArgValue,
    Body,
    BodyItem,
    FieldDef,
    CodeType,
    FieldLet,
    BitType,
    IntType,
    StringType,
    DagType,
    BitsType,
    ListType,
    ClassId,
    Value,
    InnerValue,
    RangeSuffix,
    RangeList,
    RangePiece,
    SliceSuffix,
    SliceElements,
    SliceElement,
    FieldSuffix,
    Integer,
    String,
    Code,
    Boolean,
    Uninitialized,
    Bits,
    ValueList,
    List,
    Dag,
    DagArgList,
    DagArg,
    VarName,
    Identifier,
    ClassValue,
    BangOperator,
    CondOperator,
    CondClause,

    // --- token ---
    // Markers
    Eof,
    Whitespace,
    LineComment,
    BlockComment,

    // Symbols
    Minus,
    Plus,
    LSquare,
    RSquare,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Less,
    Greater,
    Colon,
    Semi,
    Comma,
    Dot,
    Equal,
    Question,
    Paste,
    DotDotDot,

    // Keywords
    AssertKw,
    Bit,
    BitsKw,
    ClassKw,
    CodeKw,
    DagKw,
    DefKw,
    DefmKw,
    DefsetKw,
    DefvarKw,
    ElseKw,
    Field,
    ForeachKw,
    IfKw,
    In,
    IncludeKw,
    Int,
    LetKw,
    ListKw,
    MultiClassKw,
    StringKw,
    Then,

    // Bang operators
    XConcat,
    XAdd,
    XSub,
    XMul,
    XDiv,
    XNot,
    XLog2,
    XAnd,
    XOr,
    XXor,
    XSra,
    XSrl,
    XShl,
    XListConcat,
    XListSplat,
    XStrConcat,
    XInterleave,
    XSubstr,
    XFind,
    XCast,
    XSubst,
    XForEach,
    XFilter,
    XFoldl,
    XHead,
    XTail,
    XSize,
    XEmpty,
    XIf,
    XCond,
    XEq,
    XIsA,
    XDag,
    XNe,
    XLe,
    XLt,
    XGe,
    XGt,
    XSetDagOp,
    XGetDagOp,
    XExists,
    XListRemove,
    XToLower,
    XToUpper,
    XRange,
    XGetDagArg,
    XGetDagName,
    XSetDagArg,
    XSetDagName,

    // Literals
    TrueVal,
    FalseVal,

    IntVal,
    BinaryIntVal,

    // Strings
    Id,
    StrVal,
    VarNameKw,
    CodeFragment,

    // Preprocessor tokens
    PreProcessor,

    __LAST,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl From<TokenKind> for rowan::SyntaxKind {
    fn from(kind: TokenKind) -> Self {
        let kind = match kind {
            TokenKind::Eof => SyntaxKind::Eof,
            TokenKind::Whitespace => SyntaxKind::Whitespace,
            TokenKind::LineComment => SyntaxKind::LineComment,
            TokenKind::BlockComment => SyntaxKind::BlockComment,
            TokenKind::Error => SyntaxKind::Error,
            TokenKind::PreProcessor => SyntaxKind::PreProcessor,

            TokenKind::Minus => SyntaxKind::Minus,
            TokenKind::Plus => SyntaxKind::Plus,
            TokenKind::LSquare => SyntaxKind::LSquare,
            TokenKind::RSquare => SyntaxKind::RSquare,
            TokenKind::LBrace => SyntaxKind::LBrace,
            TokenKind::RBrace => SyntaxKind::RBrace,
            TokenKind::LParen => SyntaxKind::LParen,
            TokenKind::RParen => SyntaxKind::RParen,
            TokenKind::Less => SyntaxKind::Less,
            TokenKind::Greater => SyntaxKind::Greater,
            TokenKind::Colon => SyntaxKind::Colon,
            TokenKind::Semi => SyntaxKind::Semi,
            TokenKind::Comma => SyntaxKind::Comma,
            TokenKind::Dot => SyntaxKind::Dot,
            TokenKind::Equal => SyntaxKind::Equal,
            TokenKind::Question => SyntaxKind::Question,
            TokenKind::Paste => SyntaxKind::Paste,
            TokenKind::DotDotDot => SyntaxKind::DotDotDot,

            TokenKind::Assert => SyntaxKind::AssertKw,
            TokenKind::Bit => SyntaxKind::Bit,
            TokenKind::Bits => SyntaxKind::BitsKw,
            TokenKind::Class => SyntaxKind::ClassKw,
            TokenKind::Code => SyntaxKind::CodeKw,
            TokenKind::Dag => SyntaxKind::DagKw,
            TokenKind::Def => SyntaxKind::DefKw,
            TokenKind::Defm => SyntaxKind::DefmKw,
            TokenKind::Defset => SyntaxKind::DefsetKw,
            TokenKind::Defvar => SyntaxKind::DefvarKw,
            TokenKind::ElseKw => SyntaxKind::ElseKw,
            TokenKind::Field => SyntaxKind::Field,
            TokenKind::Foreach => SyntaxKind::ForeachKw,
            TokenKind::If => SyntaxKind::IfKw,
            TokenKind::In => SyntaxKind::In,
            TokenKind::Include => SyntaxKind::IncludeKw,
            TokenKind::Int => SyntaxKind::Int,
            TokenKind::Let => SyntaxKind::LetKw,
            TokenKind::List => SyntaxKind::ListKw,
            TokenKind::MultiClass => SyntaxKind::MultiClassKw,
            TokenKind::String => SyntaxKind::StringKw,
            TokenKind::Then => SyntaxKind::Then,

            TokenKind::XConcat => SyntaxKind::XConcat,
            TokenKind::XAdd => SyntaxKind::XAdd,
            TokenKind::XSub => SyntaxKind::XSub,
            TokenKind::XMul => SyntaxKind::XMul,
            TokenKind::XDiv => SyntaxKind::XDiv,
            TokenKind::XNot => SyntaxKind::XNot,
            TokenKind::XLog2 => SyntaxKind::XLog2,
            TokenKind::XAnd => SyntaxKind::XAnd,
            TokenKind::XOr => SyntaxKind::XOr,
            TokenKind::XXor => SyntaxKind::XXor,
            TokenKind::XSra => SyntaxKind::XSra,
            TokenKind::XSrl => SyntaxKind::XSrl,
            TokenKind::XShl => SyntaxKind::XShl,
            TokenKind::XListConcat => SyntaxKind::XListConcat,
            TokenKind::XListSplat => SyntaxKind::XListSplat,
            TokenKind::XStrConcat => SyntaxKind::XStrConcat,
            TokenKind::XInterleave => SyntaxKind::XInterleave,
            TokenKind::XSubstr => SyntaxKind::XSubstr,
            TokenKind::XFind => SyntaxKind::XFind,
            TokenKind::XCast => SyntaxKind::XCast,
            TokenKind::XSubst => SyntaxKind::XSubst,
            TokenKind::XForEach => SyntaxKind::XForEach,
            TokenKind::XFilter => SyntaxKind::XFilter,
            TokenKind::XFoldl => SyntaxKind::XFoldl,
            TokenKind::XHead => SyntaxKind::XHead,
            TokenKind::XTail => SyntaxKind::XTail,
            TokenKind::XSize => SyntaxKind::XSize,
            TokenKind::XEmpty => SyntaxKind::XEmpty,
            TokenKind::XIf => SyntaxKind::XIf,
            TokenKind::XCond => SyntaxKind::XCond,
            TokenKind::XEq => SyntaxKind::XEq,
            TokenKind::XIsA => SyntaxKind::XIsA,
            TokenKind::XDag => SyntaxKind::XDag,
            TokenKind::XNe => SyntaxKind::XNe,
            TokenKind::XLe => SyntaxKind::XLe,
            TokenKind::XLt => SyntaxKind::XLt,
            TokenKind::XGe => SyntaxKind::XGe,
            TokenKind::XGt => SyntaxKind::XGt,
            TokenKind::XSetDagOp => SyntaxKind::XSetDagOp,
            TokenKind::XGetDagOp => SyntaxKind::XGetDagOp,
            TokenKind::XExists => SyntaxKind::XExists,
            TokenKind::XListRemove => SyntaxKind::XListRemove,
            TokenKind::XToLower => SyntaxKind::XToLower,
            TokenKind::XToUpper => SyntaxKind::XToUpper,
            TokenKind::XRange => SyntaxKind::XRange,
            TokenKind::XGetDagArg => SyntaxKind::XGetDagArg,
            TokenKind::XGetDagName => SyntaxKind::XGetDagName,
            TokenKind::XSetDagArg => SyntaxKind::XSetDagArg,
            TokenKind::XSetDagName => SyntaxKind::XSetDagName,

            TokenKind::TrueVal => SyntaxKind::TrueVal,
            TokenKind::FalseVal => SyntaxKind::FalseVal,
            TokenKind::IntVal => SyntaxKind::IntVal,
            TokenKind::BinaryIntVal => SyntaxKind::BinaryIntVal,

            TokenKind::Id => SyntaxKind::Id,
            TokenKind::StrVal => SyntaxKind::StrVal,
            TokenKind::VarName => SyntaxKind::VarNameKw,
            TokenKind::CodeFragment => SyntaxKind::CodeFragment,

            TokenKind::Ifdef => SyntaxKind::PreProcessor,
            TokenKind::Ifndef => SyntaxKind::PreProcessor,
            TokenKind::Else => SyntaxKind::PreProcessor,
            TokenKind::Endif => SyntaxKind::PreProcessor,
            TokenKind::Define => SyntaxKind::PreProcessor,
        };
        kind.into()
    }
}
