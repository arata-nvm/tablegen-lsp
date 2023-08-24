#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Markers
    Eof,
    Whitespace,
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

    // Literals
    IntVal,

    // Strings
    Id,
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
}
