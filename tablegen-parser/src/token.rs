#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Markers
    Eof,
    Whitespace,
    LineComment,
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

#[macro_export]
macro_rules! T {
    [-] => {$crate::token::TokenKind::Minus};
    [+] => {$crate::token::TokenKind::Plus};
    ['['] => {$crate::token::TokenKind::LSquare};
    [']'] => {$crate::token::TokenKind::RSquare};
    ['{'] => {$crate::token::TokenKind::LBrace};
    ['}'] => {$crate::token::TokenKind::RBrace};
    ['('] => {$crate::token::TokenKind::LParen};
    [')'] => {$crate::token::TokenKind::RParen};
    [<] => {$crate::token::TokenKind::Less};
    [>] => {$crate::token::TokenKind::Greater};
    [:] => {$crate::token::TokenKind::Colon};
    [;] => {$crate::token::TokenKind::Semi};
    [,] => {$crate::token::TokenKind::Comma};
    [.] => {$crate::token::TokenKind::Dot};
    [=] => {$crate::token::TokenKind::Equal};
    [?] => {$crate::token::TokenKind::Question};
    [#] => {$crate::token::TokenKind::Paste};
    [...] => {$crate::token::TokenKind::DotDotDot};

    [assert] => {$crate::token::TokenKind::Assert};
    [bit] => {$crate::token::TokenKind::Bit};
    [bits] => {$crate::token::TokenKind::Bits};
    [class] => {$crate::token::TokenKind::Class};
    [code] => {$crate::token::TokenKind::Code};
    [dag] => {$crate::token::TokenKind::Dag};
    [def] => {$crate::token::TokenKind::Def};
    [defm] => {$crate::token::TokenKind::Defm};
    [defset] => {$crate::token::TokenKind::Defset};
    [defvar] => {$crate::token::TokenKind::Defvar};
    [else] => {$crate::token::TokenKind::ElseKw};
    [field] => {$crate::token::TokenKind::Field};
    [foreach] => {$crate::token::TokenKind::Foreach};
    [if] => {$crate::token::TokenKind::If};
    [in] => {$crate::token::TokenKind::In};
    [include] => {$crate::token::TokenKind::Include};
    [int] => {$crate::token::TokenKind::Int};
    [let] => {$crate::token::TokenKind::Let};
    [list] => {$crate::token::TokenKind::List};
    [multiclass] => {$crate::token::TokenKind::MultiClass};
    [string] => {$crate::token::TokenKind::String};
    [then] => {$crate::token::TokenKind::Then};

    [!concat] => {$crate::token::TokenKind::XConcat};
    [!add] => {$crate::token::TokenKind::XAdd};
    [!sub] => {$crate::token::TokenKind::XSub};
    [!mul] => {$crate::token::TokenKind::XMul};
    [!div] => {$crate::token::TokenKind::XDiv};
    [!not] => {$crate::token::TokenKind::XNot};
    [!log2] => {$crate::token::TokenKind::XLog2};
    [!and] => {$crate::token::TokenKind::XAnd};
    [!or] => {$crate::token::TokenKind::XOr};
    [!xor] => {$crate::token::TokenKind::XXor};
    [!sra] => {$crate::token::TokenKind::XSra};
    [!srl] => {$crate::token::TokenKind::XSrl};
    [!shl] => {$crate::token::TokenKind::XShl};
    [!listconcat] => {$crate::token::TokenKind::XListConcat};
    [!listsplat] => {$crate::token::TokenKind::XListSplat};
    [!strconcat] => {$crate::token::TokenKind::XStrConcat};
    [!interleave] => {$crate::token::TokenKind::XInterleave};
    [!substr] => {$crate::token::TokenKind::XSubstr};
    [!find] => {$crate::token::TokenKind::XFind};
    [!cast] => {$crate::token::TokenKind::XCast};
    [!subst] => {$crate::token::TokenKind::XSubst};
    [!foreach] => {$crate::token::TokenKind::XForEach};
    [!filter] => {$crate::token::TokenKind::XFilter};
    [!foldl] => {$crate::token::TokenKind::XFoldl};
    [!head] => {$crate::token::TokenKind::XHead};
    [!tail] => {$crate::token::TokenKind::XTail};
    [!size] => {$crate::token::TokenKind::XSize};
    [!empty] => {$crate::token::TokenKind::XEmpty};
    [!if] => {$crate::token::TokenKind::XIf};
    [!cond] => {$crate::token::TokenKind::XCond};
    [!eq] => {$crate::token::TokenKind::XEq};
    [!isa] => {$crate::token::TokenKind::XIsA};
    [!dag] => {$crate::token::TokenKind::XDag};
    [!ne] => {$crate::token::TokenKind::XNe};
    [!le] => {$crate::token::TokenKind::XLe};
    [!lt] => {$crate::token::TokenKind::XLt};
    [!ge] => {$crate::token::TokenKind::XGe};
    [!gt] => {$crate::token::TokenKind::XGt};
    [!setdagop] => {$crate::token::TokenKind::XSetDagOp};
    [!getdagop] => {$crate::token::TokenKind::XGetDagOp};
    [!exists] => {$crate::token::TokenKind::XExists};
    [!listremove] => {$crate::token::TokenKind::XListRemove};
    [!tolower] => {$crate::token::TokenKind::XToLower};
    [!toupper] => {$crate::token::TokenKind::XToUpper};
    [!range] => {$crate::token::TokenKind::XRange};
    [!getdagarg] => {$crate::token::TokenKind::XGetDagArg};
    [!getdagname] => {$crate::token::TokenKind::XGetDagName};
    [!setdagarg] => {$crate::token::TokenKind::XSetDagArg};
    [!setdagname] => {$crate::token::TokenKind::XSetDagName};

    [true] => {$crate::token::TokenKind::TrueVal};
    [false] => {$crate::token::TokenKind::FalseVal};

    [#ifdef] => {$crate::token::TokenKind::Ifdef};
    [#ifndef] => {$crate::token::TokenKind::Ifndef};
    [#else] => {$crate::token::TokenKind::Else};
    [#endif] => {$crate::token::TokenKind::Endif};
    [#define] => {$crate::token::TokenKind::Define};
}
