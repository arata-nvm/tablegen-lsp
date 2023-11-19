use crate::{grammar::value, parser::Parser, syntax_kind::SyntaxKind, token_kind::TokenKind, T};

pub(super) const TYPE_FIRST_TOKENS: [TokenKind; 8] = [
    T![bit],
    T![int],
    T![string],
    T![dag],
    T![bits],
    T![list],
    T![code],
    TokenKind::Id,
];

// Type ::= BitType | IntType | StringType | DagType | BitsType | ListType | ClassId
pub(super) fn r#type(p: &mut Parser) {
    match p.current() {
        T![bit] => bit_type(p),
        T![int] => int_type(p),
        T![string] => string_type(p),
        T![dag] => dag_type(p),
        T![bits] => bits_type(p),
        T![list] => list_type(p),
        TokenKind::Id => class_id(p),
        _ => p.error_and_recover("unknown token when expecting a type"),
    };
}

// BitType ::= "bit"
pub(super) fn bit_type(p: &mut Parser) {
    p.start_node(SyntaxKind::BitType);
    p.assert(T![bit]);
    p.finish_node();
}

// IntType ::= "int"
pub(super) fn int_type(p: &mut Parser) {
    p.start_node(SyntaxKind::IntType);
    p.assert(T![int]);
    p.finish_node();
}

// StringType ::= "string"
pub(super) fn string_type(p: &mut Parser) {
    p.start_node(SyntaxKind::StringType);
    p.assert(T![string]);
    p.finish_node();
}

// DagType ::= "dag"
pub(super) fn dag_type(p: &mut Parser) {
    p.start_node(SyntaxKind::DagType);
    p.assert(T![dag]);
    p.finish_node();
}

// BitsType ::= "bits" "<" Integer ">"
pub(super) fn bits_type(p: &mut Parser) {
    p.start_node(SyntaxKind::BitsType);
    p.assert(T![bits]);
    p.expect_with_msg(T![<], "expected '<' after bits type");
    value::integer(p).or_error(p, "expected integer in bits<n> type");
    p.expect_with_msg(T![>], "expected '>' at end of bits<n> type");
    p.finish_node();
}

// ListType ::= "list" "<" Type ">"
pub(super) fn list_type(p: &mut Parser) {
    p.start_node(SyntaxKind::ListType);
    p.assert(T![list]);
    p.expect_with_msg(T![<], "expected '<' after list type");
    r#type(p);
    p.expect_with_msg(T![>], "expected '>' at end of list<ty> type");
    p.finish_node();
}

// ClassId ::= Identifier
pub(super) fn class_id(p: &mut Parser) {
    p.start_node(SyntaxKind::ClassId);
    value::identifier(p).or_error(p, "expected name for ClassID");
    p.finish_node();
}

// CodeType ::= "code"
pub(super) fn code_type(p: &mut Parser) {
    p.start_node(SyntaxKind::CodeType);
    p.assert(T![code]);
    p.finish_node();
}
