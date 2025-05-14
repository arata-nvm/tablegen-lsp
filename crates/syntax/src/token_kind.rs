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
    Deftype,
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
    XAdd,
    XAnd,
    XCast,
    XCon,
    XCond,
    XDag,
    XDiv,
    XEmpty,
    XEq,
    XExists,
    XFilter,
    XFind,
    XFoldl,
    XForEach,
    XGe,
    XGetDagArg,
    XGetDagName,
    XGetDagOp,
    XGetOp,
    XGt,
    XHead,
    XIf,
    XInitialized,
    XInstances,
    XInterleave,
    XIsA,
    XLe,
    XListConcat,
    XListFlatten,
    XListRemove,
    XListSplat,
    XLog2,
    XLt,
    XMatch,
    XMul,
    XNe,
    XNot,
    XOr,
    XRange,
    XRepr,
    XSetDagArg,
    XSetDagName,
    XSetDagOp,
    XSetOp,
    XShl,
    XSize,
    XSra,
    XSrl,
    XStrConcat,
    XSub,
    XSubst,
    XSubstr,
    XTail,
    XToLower,
    XToUpper,
    XXor,

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
            Self::XAdd
                | Self::XAnd
                | Self::XCast
                | Self::XCon
                | Self::XDag
                | Self::XDiv
                | Self::XEmpty
                | Self::XEq
                | Self::XExists
                | Self::XFilter
                | Self::XFind
                | Self::XFoldl
                | Self::XForEach
                | Self::XGe
                | Self::XGetDagArg
                | Self::XGetDagName
                | Self::XGetDagOp
                | Self::XGetOp
                | Self::XGt
                | Self::XHead
                | Self::XIf
                | Self::XInitialized
                | Self::XInstances
                | Self::XInterleave
                | Self::XIsA
                | Self::XLe
                | Self::XListConcat
                | Self::XListFlatten
                | Self::XListRemove
                | Self::XListSplat
                | Self::XLog2
                | Self::XLt
                | Self::XMatch
                | Self::XMul
                | Self::XNe
                | Self::XNot
                | Self::XOr
                | Self::XRange
                | Self::XRepr
                | Self::XSetDagArg
                | Self::XSetDagName
                | Self::XSetDagOp
                | Self::XSetOp
                | Self::XShl
                | Self::XSize
                | Self::XSra
                | Self::XSrl
                | Self::XStrConcat
                | Self::XSub
                | Self::XSubst
                | Self::XSubstr
                | Self::XTail
                | Self::XToLower
                | Self::XToUpper
                | Self::XXor
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
    [deftype] => {$crate::token_kind::TokenKind::Deftype};
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

    [!add] => {$crate::token_kind::TokenKind::XAdd};
    [!and] => {$crate::token_kind::TokenKind::XAnd};
    [!cast] => {$crate::token_kind::TokenKind::XCast};
    [!con] => {$crate::token_kind::TokenKind::XCon};
    [!cond] => {$crate::token_kind::TokenKind::XCond};
    [!dag] => {$crate::token_kind::TokenKind::XDag};
    [!div] => {$crate::token_kind::TokenKind::XDiv};
    [!empty] => {$crate::token_kind::TokenKind::XEmpty};
    [!eq] => {$crate::token_kind::TokenKind::XEq};
    [!exists] => {$crate::token_kind::TokenKind::XExists};
    [!filter] => {$crate::token_kind::TokenKind::XFilter};
    [!find] => {$crate::token_kind::TokenKind::XFind};
    [!foldl] => {$crate::token_kind::TokenKind::XFoldl};
    [!foreach] => {$crate::token_kind::TokenKind::XForEach};
    [!ge] => {$crate::token_kind::TokenKind::XGe};
    [!getdagarg] => {$crate::token_kind::TokenKind::XGetDagArg};
    [!getdagname] => {$crate::token_kind::TokenKind::XGetDagName};
    [!getdagop] => {$crate::token_kind::TokenKind::XGetDagOp};
    [!getop] => {$crate::token_kind::TokenKind::XGetOp};
    [!gt] => {$crate::token_kind::TokenKind::XGt};
    [!head] => {$crate::token_kind::TokenKind::XHead};
    [!if] => {$crate::token_kind::TokenKind::XIf};
    [!initialized] => {$crate::token_kind::TokenKind::XInitialized};
    [!instances] => {$crate::token_kind::TokenKind::XInstances};
    [!interleave] => {$crate::token_kind::TokenKind::XInterleave};
    [!isa] => {$crate::token_kind::TokenKind::XIsA};
    [!le] => {$crate::token_kind::TokenKind::XLe};
    [!listconcat] => {$crate::token_kind::TokenKind::XListConcat};
    [!listflatten] => {$crate::token_kind::TokenKind::XListFlatten};
    [!listremove] => {$crate::token_kind::TokenKind::XListRemove};
    [!listsplat] => {$crate::token_kind::TokenKind::XListSplat};
    [!log2] => {$crate::token_kind::TokenKind::XLog2};
    [!lt] => {$crate::token_kind::TokenKind::XLt};
    [!match] => {$crate::token_kind::TokenKind::XMatch};
    [!mul] => {$crate::token_kind::TokenKind::XMul};
    [!ne] => {$crate::token_kind::TokenKind::XNe};
    [!not] => {$crate::token_kind::TokenKind::XNot};
    [!or] => {$crate::token_kind::TokenKind::XOr};
    [!range] => {$crate::token_kind::TokenKind::XRange};
    [!repr] => {$crate::token_kind::TokenKind::XRepr};
    [!setdagarg] => {$crate::token_kind::TokenKind::XSetDagArg};
    [!setdagname] => {$crate::token_kind::TokenKind::XSetDagName};
    [!setdagop] => {$crate::token_kind::TokenKind::XSetDagOp};
    [!setop] => {$crate::token_kind::TokenKind::XSetOp};
    [!shl] => {$crate::token_kind::TokenKind::XShl};
    [!size] => {$crate::token_kind::TokenKind::XSize};
    [!sra] => {$crate::token_kind::TokenKind::XSra};
    [!srl] => {$crate::token_kind::TokenKind::XSrl};
    [!strconcat] => {$crate::token_kind::TokenKind::XStrConcat};
    [!sub] => {$crate::token_kind::TokenKind::XSub};
    [!subst] => {$crate::token_kind::TokenKind::XSubst};
    [!substr] => {$crate::token_kind::TokenKind::XSubstr};
    [!tail] => {$crate::token_kind::TokenKind::XTail};
    [!tolower] => {$crate::token_kind::TokenKind::XToLower};
    [!toupper] => {$crate::token_kind::TokenKind::XToUpper};
    [!xor] => {$crate::token_kind::TokenKind::XXor};

    [true] => {$crate::token_kind::TokenKind::TrueVal};
    [false] => {$crate::token_kind::TokenKind::FalseVal};

    [#ifdef] => {$crate::token_kind::TokenKind::Ifdef};
    [#ifndef] => {$crate::token_kind::TokenKind::Ifndef};
    [#else] => {$crate::token_kind::TokenKind::Else};
    [#endif] => {$crate::token_kind::TokenKind::Endif};
    [#define] => {$crate::token_kind::TokenKind::Define};
}
