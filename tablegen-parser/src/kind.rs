#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SyntaxKind {
    // Nodes
    File,
    Class,
    TemplateArgList,
    TemplateArgDecl,
    RecordBody,
    ParentClassList,
    ClassRef,
    ArgValueList,
    PositionalArgValueList,
    Body,
    BodyItem,
    Type,
    Value,
    SimpleValue,
    Identifier,
    Integer,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    // Markers
    Eof,
    Whitespace,
    LineComment,
    BlockComment,
    Error,

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
    Assert,
    Bit,
    Bits,
    Class,
    Code,
    Dag,
    Def,
    Defm,
    Defset,
    Defvar,
    ElseKw,
    Field,
    Foreach,
    If,
    In,
    Include,
    Int,
    Let,
    List,
    MultiClass,
    String,
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
    VarName,
    CodeFragment,

    // Preprocessor tokens
    Ifdef,
    Ifndef,
    Else,
    Endif,
    Define,
}

impl TokenKind {
    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }
}

#[macro_export]
macro_rules! T {
    [-] => {$crate::kind::TokenKind::Minus};
    [+] => {$crate::kind::TokenKind::Plus};
    ['['] => {$crate::kind::TokenKind::LSquare};
    [']'] => {$crate::kind::TokenKind::RSquare};
    ['{'] => {$crate::kind::TokenKind::LBrace};
    ['}'] => {$crate::kind::TokenKind::RBrace};
    ['('] => {$crate::kind::TokenKind::LParen};
    [')'] => {$crate::kind::TokenKind::RParen};
    [<] => {$crate::kind::TokenKind::Less};
    [>] => {$crate::kind::TokenKind::Greater};
    [:] => {$crate::kind::TokenKind::Colon};
    [;] => {$crate::kind::TokenKind::Semi};
    [,] => {$crate::kind::TokenKind::Comma};
    [.] => {$crate::kind::TokenKind::Dot};
    [=] => {$crate::kind::TokenKind::Equal};
    [?] => {$crate::kind::TokenKind::Question};
    [#] => {$crate::kind::TokenKind::Paste};
    [...] => {$crate::kind::TokenKind::DotDotDot};

    [assert] => {$crate::kind::TokenKind::Assert};
    [bit] => {$crate::kind::TokenKind::Bit};
    [bits] => {$crate::kind::TokenKind::Bits};
    [class] => {$crate::kind::TokenKind::Class};
    [code] => {$crate::kind::TokenKind::Code};
    [dag] => {$crate::kind::TokenKind::Dag};
    [def] => {$crate::kind::TokenKind::Def};
    [defm] => {$crate::kind::TokenKind::Defm};
    [defset] => {$crate::kind::TokenKind::Defset};
    [defvar] => {$crate::kind::TokenKind::Defvar};
    [else] => {$crate::kind::TokenKind::ElseKw};
    [field] => {$crate::kind::TokenKind::Field};
    [foreach] => {$crate::kind::TokenKind::Foreach};
    [if] => {$crate::kind::TokenKind::If};
    [in] => {$crate::kind::TokenKind::In};
    [include] => {$crate::kind::TokenKind::Include};
    [int] => {$crate::kind::TokenKind::Int};
    [let] => {$crate::kind::TokenKind::Let};
    [list] => {$crate::kind::TokenKind::List};
    [multiclass] => {$crate::kind::TokenKind::MultiClass};
    [string] => {$crate::kind::TokenKind::String};
    [then] => {$crate::kind::TokenKind::Then};

    [!concat] => {$crate::kind::TokenKind::XConcat};
    [!add] => {$crate::kind::TokenKind::XAdd};
    [!sub] => {$crate::kind::TokenKind::XSub};
    [!mul] => {$crate::kind::TokenKind::XMul};
    [!div] => {$crate::kind::TokenKind::XDiv};
    [!not] => {$crate::kind::TokenKind::XNot};
    [!log2] => {$crate::kind::TokenKind::XLog2};
    [!and] => {$crate::kind::TokenKind::XAnd};
    [!or] => {$crate::kind::TokenKind::XOr};
    [!xor] => {$crate::kind::TokenKind::XXor};
    [!sra] => {$crate::kind::TokenKind::XSra};
    [!srl] => {$crate::kind::TokenKind::XSrl};
    [!shl] => {$crate::kind::TokenKind::XShl};
    [!listconcat] => {$crate::kind::TokenKind::XListConcat};
    [!listsplat] => {$crate::kind::TokenKind::XListSplat};
    [!strconcat] => {$crate::kind::TokenKind::XStrConcat};
    [!interleave] => {$crate::kind::TokenKind::XInterleave};
    [!substr] => {$crate::kind::TokenKind::XSubstr};
    [!find] => {$crate::kind::TokenKind::XFind};
    [!cast] => {$crate::kind::TokenKind::XCast};
    [!subst] => {$crate::kind::TokenKind::XSubst};
    [!foreach] => {$crate::kind::TokenKind::XForEach};
    [!filter] => {$crate::kind::TokenKind::XFilter};
    [!foldl] => {$crate::kind::TokenKind::XFoldl};
    [!head] => {$crate::kind::TokenKind::XHead};
    [!tail] => {$crate::kind::TokenKind::XTail};
    [!size] => {$crate::kind::TokenKind::XSize};
    [!empty] => {$crate::kind::TokenKind::XEmpty};
    [!if] => {$crate::kind::TokenKind::XIf};
    [!cond] => {$crate::kind::TokenKind::XCond};
    [!eq] => {$crate::kind::TokenKind::XEq};
    [!isa] => {$crate::kind::TokenKind::XIsA};
    [!dag] => {$crate::kind::TokenKind::XDag};
    [!ne] => {$crate::kind::TokenKind::XNe};
    [!le] => {$crate::kind::TokenKind::XLe};
    [!lt] => {$crate::kind::TokenKind::XLt};
    [!ge] => {$crate::kind::TokenKind::XGe};
    [!gt] => {$crate::kind::TokenKind::XGt};
    [!setdagop] => {$crate::kind::TokenKind::XSetDagOp};
    [!getdagop] => {$crate::kind::TokenKind::XGetDagOp};
    [!exists] => {$crate::kind::TokenKind::XExists};
    [!listremove] => {$crate::kind::TokenKind::XListRemove};
    [!tolower] => {$crate::kind::TokenKind::XToLower};
    [!toupper] => {$crate::kind::TokenKind::XToUpper};
    [!range] => {$crate::kind::TokenKind::XRange};
    [!getdagarg] => {$crate::kind::TokenKind::XGetDagArg};
    [!getdagname] => {$crate::kind::TokenKind::XGetDagName};
    [!setdagarg] => {$crate::kind::TokenKind::XSetDagArg};
    [!setdagname] => {$crate::kind::TokenKind::XSetDagName};

    [true] => {$crate::kind::TokenKind::TrueVal};
    [false] => {$crate::kind::TokenKind::FalseVal};

    [#ifdef] => {$crate::kind::TokenKind::Ifdef};
    [#ifndef] => {$crate::kind::TokenKind::Ifndef};
    [#else] => {$crate::kind::TokenKind::Else};
    [#endif] => {$crate::kind::TokenKind::Endif};
    [#define] => {$crate::kind::TokenKind::Define};
}
