#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    // Markers
    Eof,
    Whitespace,
    LineComment,
    BlockComment,
    Error,
    PreProcessor,

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
    Dump,
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
            Self::Whitespace | Self::LineComment | Self::BlockComment | Self::PreProcessor
        )
    }

    pub fn is_bang_operator(&self) -> bool {
        matches!(
            self,
            Self::XConcat
                | Self::XAdd
                | Self::XSub
                | Self::XMul
                | Self::XDiv
                | Self::XNot
                | Self::XLog2
                | Self::XAnd
                | Self::XOr
                | Self::XXor
                | Self::XSra
                | Self::XSrl
                | Self::XShl
                | Self::XListConcat
                | Self::XListSplat
                | Self::XStrConcat
                | Self::XInterleave
                | Self::XSubstr
                | Self::XFind
                | Self::XCast
                | Self::XSubst
                | Self::XForEach
                | Self::XFilter
                | Self::XFoldl
                | Self::XHead
                | Self::XTail
                | Self::XSize
                | Self::XEmpty
                | Self::XIf
                | Self::XEq
                | Self::XIsA
                | Self::XDag
                | Self::XNe
                | Self::XLe
                | Self::XLt
                | Self::XGe
                | Self::XGt
                | Self::XSetDagOp
                | Self::XGetDagOp
                | Self::XExists
                | Self::XListRemove
                | Self::XToLower
                | Self::XToUpper
                | Self::XRange
                | Self::XGetDagArg
                | Self::XGetDagName
                | Self::XSetDagArg
                | Self::XSetDagName
        )
    }

    pub fn is_cond_operator(&self) -> bool {
        matches!(self, Self::XCond)
    }
}

#[macro_export]
#[allow(non_snake_case)]
macro_rules! T {
    [-] => {$crate::token_kind::TokenKind::Minus};
    [+] => {$crate::token_kind::TokenKind::Plus};
    ['['] => {$crate::token_kind::TokenKind::LSquare};
    [']'] => {$crate::token_kind::TokenKind::RSquare};
    ['{'] => {$crate::token_kind::TokenKind::LBrace};
    ['}'] => {$crate::token_kind::TokenKind::RBrace};
    ['('] => {$crate::token_kind::TokenKind::LParen};
    [')'] => {$crate::token_kind::TokenKind::RParen};
    [<] => {$crate::token_kind::TokenKind::Less};
    [>] => {$crate::token_kind::TokenKind::Greater};
    [:] => {$crate::token_kind::TokenKind::Colon};
    [;] => {$crate::token_kind::TokenKind::Semi};
    [,] => {$crate::token_kind::TokenKind::Comma};
    [.] => {$crate::token_kind::TokenKind::Dot};
    [=] => {$crate::token_kind::TokenKind::Equal};
    [?] => {$crate::token_kind::TokenKind::Question};
    [#] => {$crate::token_kind::TokenKind::Paste};
    [...] => {$crate::token_kind::TokenKind::DotDotDot};

    [assert] => {$crate::token_kind::TokenKind::Assert};
    [bit] => {$crate::token_kind::TokenKind::Bit};
    [bits] => {$crate::token_kind::TokenKind::Bits};
    [class] => {$crate::token_kind::TokenKind::Class};
    [code] => {$crate::token_kind::TokenKind::Code};
    [dag] => {$crate::token_kind::TokenKind::Dag};
    [def] => {$crate::token_kind::TokenKind::Def};
    [defm] => {$crate::token_kind::TokenKind::Defm};
    [defset] => {$crate::token_kind::TokenKind::Defset};
    [defvar] => {$crate::token_kind::TokenKind::Defvar};
    [dump] => {$crate::token_kind::TokenKind::Dump};
    [else] => {$crate::token_kind::TokenKind::ElseKw};
    [field] => {$crate::token_kind::TokenKind::Field};
    [foreach] => {$crate::token_kind::TokenKind::Foreach};
    [if] => {$crate::token_kind::TokenKind::If};
    [in] => {$crate::token_kind::TokenKind::In};
    [include] => {$crate::token_kind::TokenKind::Include};
    [int] => {$crate::token_kind::TokenKind::Int};
    [let] => {$crate::token_kind::TokenKind::Let};
    [list] => {$crate::token_kind::TokenKind::List};
    [multiclass] => {$crate::token_kind::TokenKind::MultiClass};
    [string] => {$crate::token_kind::TokenKind::String};
    [then] => {$crate::token_kind::TokenKind::Then};

    [!concat] => {$crate::token_kind::TokenKind::XConcat};
    [!add] => {$crate::token_kind::TokenKind::XAdd};
    [!sub] => {$crate::token_kind::TokenKind::XSub};
    [!mul] => {$crate::token_kind::TokenKind::XMul};
    [!div] => {$crate::token_kind::TokenKind::XDiv};
    [!not] => {$crate::token_kind::TokenKind::XNot};
    [!log2] => {$crate::token_kind::TokenKind::XLog2};
    [!and] => {$crate::token_kind::TokenKind::XAnd};
    [!or] => {$crate::token_kind::TokenKind::XOr};
    [!xor] => {$crate::token_kind::TokenKind::XXor};
    [!sra] => {$crate::token_kind::TokenKind::XSra};
    [!srl] => {$crate::token_kind::TokenKind::XSrl};
    [!shl] => {$crate::token_kind::TokenKind::XShl};
    [!listconcat] => {$crate::token_kind::TokenKind::XListConcat};
    [!listsplat] => {$crate::token_kind::TokenKind::XListSplat};
    [!strconcat] => {$crate::token_kind::TokenKind::XStrConcat};
    [!interleave] => {$crate::token_kind::TokenKind::XInterleave};
    [!substr] => {$crate::token_kind::TokenKind::XSubstr};
    [!find] => {$crate::token_kind::TokenKind::XFind};
    [!cast] => {$crate::token_kind::TokenKind::XCast};
    [!subst] => {$crate::token_kind::TokenKind::XSubst};
    [!foreach] => {$crate::token_kind::TokenKind::XForEach};
    [!filter] => {$crate::token_kind::TokenKind::XFilter};
    [!foldl] => {$crate::token_kind::TokenKind::XFoldl};
    [!head] => {$crate::token_kind::TokenKind::XHead};
    [!tail] => {$crate::token_kind::TokenKind::XTail};
    [!size] => {$crate::token_kind::TokenKind::XSize};
    [!empty] => {$crate::token_kind::TokenKind::XEmpty};
    [!if] => {$crate::token_kind::TokenKind::XIf};
    [!cond] => {$crate::token_kind::TokenKind::XCond};
    [!eq] => {$crate::token_kind::TokenKind::XEq};
    [!isa] => {$crate::token_kind::TokenKind::XIsA};
    [!dag] => {$crate::token_kind::TokenKind::XDag};
    [!ne] => {$crate::token_kind::TokenKind::XNe};
    [!le] => {$crate::token_kind::TokenKind::XLe};
    [!lt] => {$crate::token_kind::TokenKind::XLt};
    [!ge] => {$crate::token_kind::TokenKind::XGe};
    [!gt] => {$crate::token_kind::TokenKind::XGt};
    [!setdagop] => {$crate::token_kind::TokenKind::XSetDagOp};
    [!getdagop] => {$crate::token_kind::TokenKind::XGetDagOp};
    [!exists] => {$crate::token_kind::TokenKind::XExists};
    [!listremove] => {$crate::token_kind::TokenKind::XListRemove};
    [!tolower] => {$crate::token_kind::TokenKind::XToLower};
    [!toupper] => {$crate::token_kind::TokenKind::XToUpper};
    [!range] => {$crate::token_kind::TokenKind::XRange};
    [!getdagarg] => {$crate::token_kind::TokenKind::XGetDagArg};
    [!getdagname] => {$crate::token_kind::TokenKind::XGetDagName};
    [!setdagarg] => {$crate::token_kind::TokenKind::XSetDagArg};
    [!setdagname] => {$crate::token_kind::TokenKind::XSetDagName};

    [true] => {$crate::token_kind::TokenKind::TrueVal};
    [false] => {$crate::token_kind::TokenKind::FalseVal};

    [#ifdef] => {$crate::token_kind::TokenKind::Ifdef};
    [#ifndef] => {$crate::token_kind::TokenKind::Ifndef};
    [#else] => {$crate::token_kind::TokenKind::Else};
    [#endif] => {$crate::token_kind::TokenKind::Endif};
    [#define] => {$crate::token_kind::TokenKind::Define};
}
