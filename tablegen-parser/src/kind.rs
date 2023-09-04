#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SyntaxKind {
    // Nodes
    ClassDef,

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

impl SyntaxKind {
    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }
}

#[macro_export]
macro_rules! T {
    [-] => {$crate::kind::SyntaxKind::Minus};
    [+] => {$crate::kind::SyntaxKind::Plus};
    ['['] => {$crate::kind::SyntaxKind::LSquare};
    [']'] => {$crate::kind::SyntaxKind::RSquare};
    ['{'] => {$crate::kind::SyntaxKind::LBrace};
    ['}'] => {$crate::kind::SyntaxKind::RBrace};
    ['('] => {$crate::kind::SyntaxKind::LParen};
    [')'] => {$crate::kind::SyntaxKind::RParen};
    [<] => {$crate::kind::SyntaxKind::Less};
    [>] => {$crate::kind::SyntaxKind::Greater};
    [:] => {$crate::kind::SyntaxKind::Colon};
    [;] => {$crate::kind::SyntaxKind::Semi};
    [,] => {$crate::kind::SyntaxKind::Comma};
    [.] => {$crate::kind::SyntaxKind::Dot};
    [=] => {$crate::kind::SyntaxKind::Equal};
    [?] => {$crate::kind::SyntaxKind::Question};
    [#] => {$crate::kind::SyntaxKind::Paste};
    [...] => {$crate::kind::SyntaxKind::DotDotDot};

    [assert] => {$crate::kind::SyntaxKind::Assert};
    [bit] => {$crate::kind::SyntaxKind::Bit};
    [bits] => {$crate::kind::SyntaxKind::Bits};
    [class] => {$crate::kind::SyntaxKind::Class};
    [code] => {$crate::kind::SyntaxKind::Code};
    [dag] => {$crate::kind::SyntaxKind::Dag};
    [def] => {$crate::kind::SyntaxKind::Def};
    [defm] => {$crate::kind::SyntaxKind::Defm};
    [defset] => {$crate::kind::SyntaxKind::Defset};
    [defvar] => {$crate::kind::SyntaxKind::Defvar};
    [else] => {$crate::kind::SyntaxKind::ElseKw};
    [field] => {$crate::kind::SyntaxKind::Field};
    [foreach] => {$crate::kind::SyntaxKind::Foreach};
    [if] => {$crate::kind::SyntaxKind::If};
    [in] => {$crate::kind::SyntaxKind::In};
    [include] => {$crate::kind::SyntaxKind::Include};
    [int] => {$crate::kind::SyntaxKind::Int};
    [let] => {$crate::kind::SyntaxKind::Let};
    [list] => {$crate::kind::SyntaxKind::List};
    [multiclass] => {$crate::kind::SyntaxKind::MultiClass};
    [string] => {$crate::kind::SyntaxKind::String};
    [then] => {$crate::kind::SyntaxKind::Then};

    [!concat] => {$crate::kind::SyntaxKind::XConcat};
    [!add] => {$crate::kind::SyntaxKind::XAdd};
    [!sub] => {$crate::kind::SyntaxKind::XSub};
    [!mul] => {$crate::kind::SyntaxKind::XMul};
    [!div] => {$crate::kind::SyntaxKind::XDiv};
    [!not] => {$crate::kind::SyntaxKind::XNot};
    [!log2] => {$crate::kind::SyntaxKind::XLog2};
    [!and] => {$crate::kind::SyntaxKind::XAnd};
    [!or] => {$crate::kind::SyntaxKind::XOr};
    [!xor] => {$crate::kind::SyntaxKind::XXor};
    [!sra] => {$crate::kind::SyntaxKind::XSra};
    [!srl] => {$crate::kind::SyntaxKind::XSrl};
    [!shl] => {$crate::kind::SyntaxKind::XShl};
    [!listconcat] => {$crate::kind::SyntaxKind::XListConcat};
    [!listsplat] => {$crate::kind::SyntaxKind::XListSplat};
    [!strconcat] => {$crate::kind::SyntaxKind::XStrConcat};
    [!interleave] => {$crate::kind::SyntaxKind::XInterleave};
    [!substr] => {$crate::kind::SyntaxKind::XSubstr};
    [!find] => {$crate::kind::SyntaxKind::XFind};
    [!cast] => {$crate::kind::SyntaxKind::XCast};
    [!subst] => {$crate::kind::SyntaxKind::XSubst};
    [!foreach] => {$crate::kind::SyntaxKind::XForEach};
    [!filter] => {$crate::kind::SyntaxKind::XFilter};
    [!foldl] => {$crate::kind::SyntaxKind::XFoldl};
    [!head] => {$crate::kind::SyntaxKind::XHead};
    [!tail] => {$crate::kind::SyntaxKind::XTail};
    [!size] => {$crate::kind::SyntaxKind::XSize};
    [!empty] => {$crate::kind::SyntaxKind::XEmpty};
    [!if] => {$crate::kind::SyntaxKind::XIf};
    [!cond] => {$crate::kind::SyntaxKind::XCond};
    [!eq] => {$crate::kind::SyntaxKind::XEq};
    [!isa] => {$crate::kind::SyntaxKind::XIsA};
    [!dag] => {$crate::kind::SyntaxKind::XDag};
    [!ne] => {$crate::kind::SyntaxKind::XNe};
    [!le] => {$crate::kind::SyntaxKind::XLe};
    [!lt] => {$crate::kind::SyntaxKind::XLt};
    [!ge] => {$crate::kind::SyntaxKind::XGe};
    [!gt] => {$crate::kind::SyntaxKind::XGt};
    [!setdagop] => {$crate::kind::SyntaxKind::XSetDagOp};
    [!getdagop] => {$crate::kind::SyntaxKind::XGetDagOp};
    [!exists] => {$crate::kind::SyntaxKind::XExists};
    [!listremove] => {$crate::kind::SyntaxKind::XListRemove};
    [!tolower] => {$crate::kind::SyntaxKind::XToLower};
    [!toupper] => {$crate::kind::SyntaxKind::XToUpper};
    [!range] => {$crate::kind::SyntaxKind::XRange};
    [!getdagarg] => {$crate::kind::SyntaxKind::XGetDagArg};
    [!getdagname] => {$crate::kind::SyntaxKind::XGetDagName};
    [!setdagarg] => {$crate::kind::SyntaxKind::XSetDagArg};
    [!setdagname] => {$crate::kind::SyntaxKind::XSetDagName};

    [true] => {$crate::kind::SyntaxKind::TrueVal};
    [false] => {$crate::kind::SyntaxKind::FalseVal};

    [#ifdef] => {$crate::kind::SyntaxKind::Ifdef};
    [#ifndef] => {$crate::kind::SyntaxKind::Ifndef};
    [#else] => {$crate::kind::SyntaxKind::Else};
    [#endif] => {$crate::kind::SyntaxKind::Endif};
    [#define] => {$crate::kind::SyntaxKind::Define};
}
