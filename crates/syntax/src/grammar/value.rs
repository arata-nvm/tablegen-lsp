use crate::{
    parser::{CompletedMarker, Parser},
    syntax_kind::SyntaxKind,
    token_kind::TokenKind,
    T,
};

use super::{delimited, r#type, statement};

pub(super) const VALUE_START: [TokenKind; 63] = [
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
    T![!con],
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
    T![!initialized],
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
    T![!repr],
];

pub(super) fn opt_value(p: &mut Parser) {
    if p.at_set(&VALUE_START) {
        value(p);
    }
}

// Value ::= InnerValue ( "#" InnerValue )*
pub(super) fn value(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Value);
    inner_value(p);
    while p.eat_if(T![#]) {
        inner_value(p);
    }
    p.finish_node();
    CompletedMarker::Success
}

// InnerValue ::= SimpleValue ValueSuffix*
pub(super) fn inner_value(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::InnerValue);
    if !simple_value(p).is_success() {
        p.finish_node();
        return CompletedMarker::Fail;
    }

    while value_suffix(p) {}

    p.finish_node();
    CompletedMarker::Success
}

pub(super) fn opt_name_value(p: &mut Parser) {
    if p.at_set(&VALUE_START) {
        name_value(p);
    }
}

// Value(NameMode) ::= InnerValue ( "#" InnerValue )*
pub(super) fn name_value(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Value);
    inner_name_value(p);
    while p.eat_if(T![#]) {
        inner_name_value(p);
    }
    p.finish_node();
    CompletedMarker::Success
}

// InnerValue(NameMode) ::= SimpleValue ValueSuffix*
pub(super) fn inner_name_value(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::InnerValue);
    if !simple_value(p).is_success() {
        p.finish_node();
        return CompletedMarker::Fail;
    }

    while !p.at(T!['{']) && value_suffix(p) {}

    p.finish_node();
    CompletedMarker::Success
}

// ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
pub(super) fn value_suffix(p: &mut Parser) -> bool {
    match p.peek() {
        T!['{'] => range_suffix(p),
        T!['['] => slice_suffix(p),
        T![.] => field_suffix(p),
        _ => return false,
    };

    true
}

// RangeSuffix ::= "{" RangeList "}"
pub(super) fn range_suffix(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::RangeSuffix);
    p.assert(T!['{']);
    range_list(p);
    p.expect_with_msg(T!['}'], "expected '}' at end of bit range list");
    p.finish_node();
    CompletedMarker::Success
}

// RangeList ::= RangePiece ( "," RangePiece )*
pub(super) fn range_list(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::RangeList);
    while !p.eof() {
        range_piece(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.finish_node();
    CompletedMarker::Success
}

// RangePiece ::= Integer | Integer "..." Integer | Integer "-" Integer | Integer Integer
pub(super) fn range_piece(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::RangePiece);
    integer(p).or_error(p, "expected integer or bitrange");
    if p.at_set(&[T![...], T![-]]) {
        p.eat();
    }
    if p.at(TokenKind::IntVal) {
        integer(p).or_error(p, "expected integer value as end of range");
    }
    p.finish_node();
    CompletedMarker::Success
}

// SliceSuffix ::= "[" SliceElements "]"
pub(super) fn slice_suffix(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::SliceSuffix);
    p.assert(T!['[']);
    slice_elements(p);
    p.expect_with_msg(T![']'], "expected ']' at end of list slice");
    p.finish_node();
    CompletedMarker::Success
}

// SliceElements ::= ( SliceElement "," )* SliceElement ","?
pub(super) fn slice_elements(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::SliceElements);
    while !p.eof() {
        slice_element(p);
        if !p.eat_if(T![,]) || p.at(T![']']) {
            break;
        }
    }
    p.finish_node();
    CompletedMarker::Success
}

// SliceElement ::= Value | Value "..." Value | Value "-" Value | Value Integer
pub(super) fn slice_element(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::SliceElement);
    value(p);
    if p.at_set(&[T![...], T![-]]) {
        p.eat();
    }
    opt_value(p);
    p.finish_node();
    CompletedMarker::Success
}

// FieldSuffix ::= "." Identifier
pub(super) fn field_suffix(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::FieldSuffix);
    p.assert(T![.]);
    identifier(p).or_error(p, "expected field identifier after '.'");
    p.finish_node();
    CompletedMarker::Success
}

// SimpleValue ::= Integer | String | Code | Boolean | Uninitialized | Bits | List | Dag | Identifier | ClassValue | BangOperator | CondOperator
pub(super) fn simple_value(p: &mut Parser) -> CompletedMarker {
    match p.peek() {
        TokenKind::IntVal | TokenKind::BinaryIntVal => integer(p),
        TokenKind::StrVal => string(p),
        TokenKind::CodeFragment => code(p),
        T![true] | T![false] => boolean(p),
        T![?] => uninitialized(p),
        T!['{'] => bits(p),
        T!['['] => list(p),
        T!['('] => dag(p),
        TokenKind::Id => identifier_or_class_value(p),
        kind if kind.is_bang_operator() => bang_operator(p),
        kind if kind.is_cond_operator() => cond_operator(p),
        _ => {
            p.error_and_recover("unknown token when parsing a value");
            CompletedMarker::Fail
        }
    }
}

// Integer ::= INT
pub(super) fn integer(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Integer);
    if p.eat_if(TokenKind::IntVal) || p.eat_if(TokenKind::BinaryIntVal) {
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// String ::= STRING
pub(super) fn string(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::String);
    if p.at(TokenKind::StrVal) {
        while p.eat_if(TokenKind::StrVal) {}
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// Code ::= CODE
pub(super) fn code(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Code);
    if p.eat_if(TokenKind::CodeFragment) {
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// Boolean ::= "true" | "false"
pub(super) fn boolean(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Boolean);
    if p.eat_if(T![true]) || p.eat_if(T![false]) {
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// Uninitialized ::= "?"
pub(super) fn uninitialized(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Uninitialized);
    if p.eat_if(T![?]) {
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// Bits ::= "{" ValueList "}"
pub(super) fn bits(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Bits);
    value_list(p, T!['{'], T!['}']);
    p.finish_node();
    CompletedMarker::Success
}

// List ::= "[" ValueList "]"
pub(super) fn list(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::List);
    value_list(p, T!['['], T![']']);
    if p.eat_if(T![<]) {
        r#type::r#type(p);
        p.expect_with_msg(T![>], "expected '>' at end of list element type");
    }
    p.finish_node();
    CompletedMarker::Success
}

// ValueList ::= Value ( "," Value )*
pub(super) fn value_list(p: &mut Parser, bra: TokenKind, ket: TokenKind) -> CompletedMarker {
    p.start_node(SyntaxKind::ValueList);
    delimited(p, bra, ket, T![,], |p| {
        value(p);
    });
    p.finish_node();
    CompletedMarker::Success
}

// Dag ::= ( DagArg DagArgList? )
pub(super) fn dag(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Dag);
    p.expect(T!['(']);
    if !p.at_set(&[TokenKind::Id, T![!cast], T![?], T![!getdagop]]) {
        p.error("expected identifier in dag init");
        p.finish_node();
        return CompletedMarker::Fail;
    }
    dagarg(p);
    if !p.at(T![')']) {
        dagarg_list(p);
    }
    p.expect_with_msg(T![')'], "expected ')' in dag init");
    p.finish_node();
    CompletedMarker::Success
}

// DagArgList ::= DagArg ( "," DagArg )*
pub(super) fn dagarg_list(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::DagArgList);
    while !p.eof() {
        dagarg(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.finish_node();
    CompletedMarker::Success
}

// DagArg ::= Value ( ":" VARNAME ) | VARNAME
pub(super) fn dagarg(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::DagArg);
    if p.eat_if(TokenKind::VarName) {
        p.finish_node();
        return CompletedMarker::Success;
    }

    value(p);
    if p.eat_if(T![:]) {
        var_name(p).or_error(p, "expected variable name in dag literal");
    }
    p.finish_node();
    CompletedMarker::Success
}

// VarName ::= VARNAME
pub(super) fn var_name(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::VarName);
    if p.eat_if(TokenKind::VarName) {
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// Identifier ::= ID
pub(super) fn identifier(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::Identifier);
    if p.eat_if(TokenKind::Id) {
        p.finish_node();
        CompletedMarker::Success
    } else {
        p.finish_node();
        CompletedMarker::Fail
    }
}

// ClassValue ::= Identifier ( "<" ArgValueList? ">" )?
pub(super) fn identifier_or_class_value(p: &mut Parser) -> CompletedMarker {
    let c = p.builder().checkpoint();
    let m = identifier(p);
    if p.eat_if(T![<]) {
        p.builder().start_node_at(c, SyntaxKind::ClassValue.into());
        statement::arg_value_list(p);
        p.expect_with_msg(T![>], "expected '>' at end of value list");
        p.finish_node();
        CompletedMarker::Success
    } else {
        m
    }
}

// BangOperator ::= BANGOP ( "<" Type ">" )? "(" ValueList ")"
pub(super) fn bang_operator(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::BangOperator);
    if !p.peek().is_bang_operator() {
        p.error_and_recover("expected bang operator");
        p.finish_node();
        return CompletedMarker::Fail;
    }
    p.eat(); // eat bang operator
    if p.eat_if(T![<]) {
        r#type::r#type(p);
        p.expect(T![>]);
    }
    delimited(p, T!['('], T![')'], T![,], |p| {
        value(p);
    });
    p.finish_node();
    CompletedMarker::Success
}

// CondOperator ::= CONDOP "(" CondClause ( "," CondClause )* ")"
pub(super) fn cond_operator(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::CondOperator);
    p.expect(T![!cond]);
    delimited(p, T!['('], T![')'], T![,], |p| {
        cond_clause(p);
    });
    p.finish_node();
    CompletedMarker::Success
}

// CondClause ::= Value ":" Value
pub(super) fn cond_clause(p: &mut Parser) -> CompletedMarker {
    p.start_node(SyntaxKind::CondClause);
    value(p);
    p.expect(T![:]);
    value(p);
    p.finish_node();
    CompletedMarker::Success
}
