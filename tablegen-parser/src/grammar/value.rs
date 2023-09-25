use crate::{
    kind::{SyntaxKind, TokenKind},
    parser::{CompletedMarker, Parser},
    T,
};

use super::delimited;

// Value ::= SimpleValue ValueSuffix* | Value "#" Value?
pub(super) fn value(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if !simple_value(p).is_success() {
        return p.abandon(m);
    }

    loop {
        // ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
        match p.current() {
            T![.] => field_suffix(p),
            _ => break,
        };
    }

    p.wrap(m, SyntaxKind::Value)
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
        TokenKind::IntVal => integer(p),
        TokenKind::StrVal => string(p),
        TokenKind::CodeFragment => code(p),
        T![true] | T![false] => boolean(p),
        T![?] => uninitialized(p),
        T!['{'] => bits(p),
        T!['['] => list(p),
        T!['('] => dag(p),
        TokenKind::Id => identifier(p),
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
    if p.eat_if(TokenKind::IntVal) {
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

// BangOperator ::= BANGOP "(" ValueList ")"
pub(super) fn bang_operator(p: &mut Parser) -> CompletedMarker {
    let m = p.marker();
    if !p.current().is_bang_operator() {
        p.error_and_recover("expected bang operator");
        return p.abandon(m);
    }
    p.eat(); // eat bang operator
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