use crate::{
    kind::{SyntaxKind, TokenKind},
    parser::Parser,
    T,
};

use super::value;

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
    let m = p.marker();
    p.assert(T![bit]);
    p.wrap(m, SyntaxKind::BitType);
}

// IntType ::= "int"
pub(super) fn int_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![int]);
    p.wrap(m, SyntaxKind::IntType);
}

// StringType ::= "string"
pub(super) fn string_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![string]);
    p.wrap(m, SyntaxKind::StringType);
}

// DagType ::= "dag"
pub(super) fn dag_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![dag]);
    p.wrap(m, SyntaxKind::DagType);
}

// BitsType ::= "bits" "<" Integer ">"
pub(super) fn bits_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![bits]);
    p.expect_with_msg(T![<], "expected '<' after bits type");
    value::integer(p).or_error(p, "expected integer in bits<n> type");
    p.expect_with_msg(T![>], "expected '>' at end of bits<n> type");
    p.wrap(m, SyntaxKind::BitsType);
}

// ListType ::= "list" "<" Type ">"
pub(super) fn list_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![list]);
    p.expect_with_msg(T![<], "expected '<' after list type");
    r#type(p);
    p.expect_with_msg(T![>], "expected '>' at end of list<ty> type");
    p.wrap(m, SyntaxKind::ListType);
}

// ClassId ::= Identifier
pub(super) fn class_id(p: &mut Parser) {
    let m = p.marker();
    value::identifier(p).or_error(p, "expected name for ClassID");
    p.wrap(m, SyntaxKind::ClassId);
}
