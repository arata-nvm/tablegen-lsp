#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Markers
    Eof,

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

    // Literals
    IntVal,
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
}
