use crate::{
    kind::{SyntaxKind, TokenKind},
    parser::{CompletedMarker, Parser},
    T,
};

use super::{delimited, r#type, statement};

pub(super) const VALUE_START: [TokenKind; 61] = [
    TokenKind::IntVal,
    TokenKind::BinaryIntVal,
    TokenKind::StrVal,
    TokenKind::CodeFragment,
    T![true],
    T![false],
    T![?],
    T!['{'],
    T!['['],
    T!['('],
    TokenKind::Id,
    TokenKind::VarName,
    T![!concat],
    T![!add],
    T![!sub],
    T![!mul],
    T![!div],
    T![!not],
    T![!log2],
    T![!and],
    T![!or],
    T![!xor],
    T![!sra],
    T![!srl],
    T![!shl],
    T![!listconcat],
    T![!listsplat],
    T![!strconcat],
    T![!interleave],
    T![!substr],
    T![!find],
    T![!cast],
    T![!subst],
    T![!foreach],
    T![!filter],
    T![!foldl],
    T![!head],
    T![!tail],
    T![!size],
    T![!empty],
    T![!if],
    T![!cond],
    T![!eq],
    T![!isa],
    T![!dag],
    T![!ne],
    T![!le],
    T![!lt],
    T![!ge],
    T![!gt],
    T![!setdagop],
    T![!getdagop],
    T![!exists],
    T![!listremove],
    T![!tolower],
    T![!toupper],
    T![!range],
    T![!getdagarg],
    T![!getdagname],
    T![!setdagarg],
    T![!setdagname],
];

pub(super) fn opt_value(p: &mut Parser) {
    if p.at_set(&VALUE_START) {
        value(p);
    }
}

// Value ::= InnerValue ( "#" InnerValue )*
pub(super) fn value(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    inner_value(p);
    while p.eat_if(T![#]) {
        inner_value(p);
    }
    p.wrap(m, SyntaxKind::Value)
}

// InnerValue ::= SimpleValue ValueSuffix*
pub(super) fn inner_value(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if !simple_value(p).is_success() {
        return p.abandon(m);
    }

    while value_suffix(p) {}

    p.wrap(m, SyntaxKind::InnerValue)
}

// ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
pub(super) fn value_suffix(p: &mut Parser) -> bool {
    match p.current() {
        T!['{'] => range_suffix(p),
        T!['['] => slice_suffix(p),
        T![.] => field_suffix(p),
        _ => return false,
    };

    true
}

// RangeSuffix ::= "{" RangeList "}"
pub(super) fn range_suffix(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    p.assert(T!['{']);
    range_list(p);
    p.expect_with_msg(T!['}'], "expected '}' at end of bit range list");
    p.wrap(m, SyntaxKind::RangeSuffix)
}

// RangeList ::= RangePiece ( "," RangePiece )*
pub(super) fn range_list(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    while !p.eof() {
        range_piece(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::RangeList)
}

// RangePiece ::= Integer | Integer "..." Integer | Integer "-" Integer | Integer Integer
pub(super) fn range_piece(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    integer(p).or_error(p, "expected integer or bitrange");
    if p.at_set(&[T![...], T![-]]) {
        p.eat();
    }
    if p.at(TokenKind::IntVal) {
        integer(p).or_error(p, "expected integer value as end of range");
    }
    p.wrap(m, SyntaxKind::RangePiece)
}

// SliceSuffix ::= "[" SliceElements "]"
pub(super) fn slice_suffix(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    p.assert(T!['[']);
    slice_elements(p);
    p.expect_with_msg(T![']'], "expected ']' at end of list slice");
    p.wrap(m, SyntaxKind::SliceSuffix)
}

// SliceElements ::= ( SliceElement "," )* SliceElement ","?
pub(super) fn slice_elements(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    while !p.eof() {
        slice_element(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::SliceElements)
}

// SliceElement ::= Value | Value "..." Value | Value "-" Value | Value Integer
pub(super) fn slice_element(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    value(p);
    if p.at_set(&[T![...], T![-]]) {
        p.eat();
    }
    opt_value(p);
    p.wrap(m, SyntaxKind::SliceElement)
}

// FieldSuffix ::= "." Identifier
pub(super) fn field_suffix(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    p.assert(T![.]);
    identifier(p).or_error(p, "expected field identifier after '.'");
    p.wrap(m, SyntaxKind::FieldSuffix)
}

// SimpleValue ::= Integer | String | Code | Boolean | Uninitialized | Bits | List | Dag | Identifier | ClassValue | BangOperator | CondOperator
pub(super) fn simple_value(p: &mut Parser) -> CompletedMarker {
    match p.current() {
        TokenKind::IntVal | TokenKind::BinaryIntVal => integer(p),
        TokenKind::StrVal => string(p),
        TokenKind::CodeFragment => code(p),
        T![true] | T![false] => boolean(p),
        T![?] => uninitialized(p),
        T!['{'] => bits(p),
        T!['['] => list(p),
        T!['('] => dag(p),
        TokenKind::Id => class_value(p),
        kind if kind.is_bang_operator() => bang_operator(p),
        kind if kind.is_cond_operator() => cond_operator(p),
        _ => {
            p.error_and_recover("unknown token when parsing a value");
            CompletedMarker::fail()
        }
    }
}

// Integer ::= INT
pub(super) fn integer(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(TokenKind::IntVal) || p.eat_if(TokenKind::BinaryIntVal) {
        p.wrap(m, SyntaxKind::Integer)
    } else {
        p.abandon(m)
    }
}

// String ::= STRING
pub(super) fn string(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(TokenKind::StrVal) {
        p.wrap(m, SyntaxKind::String)
    } else {
        p.abandon(m)
    }
}

// Code ::= CODE
pub(super) fn code(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(TokenKind::CodeFragment) {
        p.wrap(m, SyntaxKind::Code)
    } else {
        p.abandon(m)
    }
}

// Boolean ::= "true" | "false"
pub(super) fn boolean(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(T![true]) || p.eat_if(T![false]) {
        p.wrap(m, SyntaxKind::Boolean)
    } else {
        p.abandon(m)
    }
}

// Uninitialized ::= "?"
pub(super) fn uninitialized(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(T![?]) {
        p.wrap(m, SyntaxKind::Uninitialized)
    } else {
        p.abandon(m)
    }
}

// Bits ::= "{" ValueList "}"
pub(super) fn bits(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    value_list(p, T!['{'], T!['}']);
    p.wrap(m, SyntaxKind::Bits)
}

// List ::= "[" ValueList "]"
pub(super) fn list(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    value_list(p, T!['['], T![']']);
    if p.eat_if(T![<]) {
        r#type::r#type(p);
        p.expect_with_msg(T![>], "expected '>' at end of list element type");
    }
    p.wrap(m, SyntaxKind::List)
}

// ValueList ::= Value ( "," Value )*
pub(super) fn value_list(p: &mut Parser, bra: TokenKind, ket: TokenKind) -> CompletedMarker {
    let m = p.marker();
    delimited(p, bra, ket, T![,], |p| {
        value(p);
    });
    p.wrap(m, SyntaxKind::ValueList)
}

// Dag ::= ( DagArg DagArgList? )
pub(super) fn dag(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    p.expect(T!['(']);
    if !p.at(TokenKind::Id) {
        p.error("expected identifier in dag init");
        return p.abandon(m);
    }
    dagarg(p);
    if !p.at(T![')']) {
        dagarg_list(p);
    }
    p.expect_with_msg(T![')'], "expected ')' in dag init");
    p.wrap(m, SyntaxKind::Dag)
}

// DagArgList ::= DagArg ( "," DagArg )*
pub(super) fn dagarg_list(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    while !p.eof() {
        dagarg(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::DagArgList)
}

// DagArg ::= Value ( ":" VARNAME ) | VARNAME
pub(super) fn dagarg(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(TokenKind::VarName) {
        return p.wrap(m, SyntaxKind::DagArg);
    }

    value(p);
    if p.eat_if(T![:]) {
        var_name(p).or_error(p, "expected variable name in dag literal");
    }
    p.wrap(m, SyntaxKind::DagArg)
}

// VarName ::= VARNAME
pub(super) fn var_name(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(TokenKind::VarName) {
        p.wrap(m, SyntaxKind::VarName)
    } else {
        p.abandon(m)
    }
}

// Identifier ::= ID
pub(super) fn identifier(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if p.eat_if(TokenKind::Id) {
        p.wrap(m, SyntaxKind::Identifier)
    } else {
        p.abandon(m)
    }
}

// ClassValue ::= Identifier ( "<" ArgValueList? ">" )?
pub(super) fn class_value(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    let m2 = identifier(p);
    if p.eat_if(T![<]) {
        statement::arg_value_list(p);
        p.expect_with_msg(T![>], "expected '>' at end of value list");
        p.wrap(m, SyntaxKind::ClassValue)
    } else {
        p.abandon(m);
        m2
    }
}

// BangOperator ::= BANGOP ( "<" Type ">" )? "(" ValueList ")"
pub(super) fn bang_operator(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if !p.current().is_bang_operator() {
        p.error_and_recover("expected bang operator");
        return p.abandon(m);
    }
    p.eat(); // eat bang operator
    if p.eat_if(T![<]) {
        r#type::r#type(p);
        p.expect(T![>]);
    }
    delimited(p, T!['('], T![')'], T![,], |p| {
        value(p);
    });
    p.wrap(m, SyntaxKind::BangOperator)
}

// CondOperator ::= CONDOP "(" CondClause ( "," CondClause )* ")"
pub(super) fn cond_operator(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    p.expect(T![!cond]);
    delimited(p, T!['('], T![')'], T![,], |p| {
        cond_clause(p);
    });
    p.wrap(m, SyntaxKind::CondOperator)
}

// CondClause ::= Value ":" Value
pub(super) fn cond_clause(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    value(p);
    p.expect(T![:]);
    value(p);
    p.wrap(m, SyntaxKind::CondClause)
}
