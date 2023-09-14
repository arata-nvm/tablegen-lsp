use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    parser::Parser,
    T,
};

pub fn parse(text: &str) -> SyntaxNode {
    let mut parser = Parser::new(text);
    file(&mut parser);
    parser.finish().into_iter().next().unwrap()
}

fn file(p: &mut Parser) {
    let m = p.marker();
    p.eat_trivia();
    while !p.eof() {
        match p.current() {
            T![include] => include(p),
            T![class] => class(p),
            T![def] => def(p),
            _ => p.error_and_eat("Expected class, def, defm, defset, multiclass, let or foreach"),
        }
    }
    p.wrap_all(m, SyntaxKind::File);
}

fn include(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![include]);
    string(p);
    p.wrap(m, SyntaxKind::Include);
}

fn class(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![class]);
    identifier(p);
    if p.at(T![<]) {
        template_arg_list(p);
    }
    record_body(p);
    p.wrap(m, SyntaxKind::Class);
}

fn def(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![def]);
    value(p);
    record_body(p);
    p.wrap(m, SyntaxKind::Def);
}

fn template_arg_list(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![<]);
    while !p.eof() && !p.at(T![>]) {
        tempalte_arg_decl(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T![>]);
    p.wrap(m, SyntaxKind::TemplateArgList);
}

fn tempalte_arg_decl(p: &mut Parser) {
    let m = p.marker();
    r#type(p);
    identifier(p);
    if p.eat_if(T![=]) {
        value(p);
    }
    p.wrap(m, SyntaxKind::TemplateArgDecl);
}

fn record_body(p: &mut Parser) {
    let m = p.marker();
    parent_class_list(p);
    body(p);
    p.wrap(m, SyntaxKind::RecordBody);
}

fn parent_class_list(p: &mut Parser) {
    let m = p.marker();
    if !p.eat_if(T![:]) {
        p.wrap(m, SyntaxKind::ParentClassList);
        return;
    }

    while !p.eof() && p.at(TokenKind::Id) {
        class_ref(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::ParentClassList);
}

fn class_ref(p: &mut Parser) {
    let m = p.marker();
    identifier(p);
    if p.eat_if(T![<]) {
        arg_value_list(p);
        p.expect(T![>]);
    }
    p.wrap(m, SyntaxKind::ClassRef);
}

fn arg_value_list(p: &mut Parser) {
    let m = p.marker();
    positional_arg_value_list(p);
    p.wrap(m, SyntaxKind::ArgValueList);
}

fn positional_arg_value_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() && !p.at(T![>]) {
        value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::PositionalArgValueList);
}

fn body(p: &mut Parser) {
    let m = p.marker();
    if p.eat_if(T![;]) {
        p.wrap(m, SyntaxKind::Body);
        return;
    }

    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        let prev_cursor = p.current_start();
        body_item(p);
        if p.current_start() == prev_cursor {
            p.error_and_eat("unexpected token");
        }
    }
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::Body);
}

fn body_item(p: &mut Parser) {
    let m = p.marker();
    r#type(p);
    identifier(p);
    if p.eat_if(T![=]) {
        value(p);
    }
    p.expect(T![;]);
    p.wrap(m, SyntaxKind::BodyItem);
}

fn r#type(p: &mut Parser) {
    let m = p.marker();
    match p.current() {
        T![bit] | T![int] | T![string] | T![dag] | T![code] => p.eat(),
        T![bits] => bits_type(p),
        T![list] => list_type(p),
        TokenKind::Id => class_ref(p),
        _ => p.error_and_eat("expected type"),
    }
    p.wrap(m, SyntaxKind::Type);
}

fn bits_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![bits]);
    p.expect(T![<]);
    integer(p);
    p.expect(T![>]);
    p.wrap(m, SyntaxKind::BitsType);
}

fn list_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![list]);
    p.expect(T![<]);
    r#type(p);
    p.expect(T![>]);
    p.wrap(m, SyntaxKind::ListType);
}

fn value(p: &mut Parser) {
    let m = p.marker();
    simple_value(p);
    p.wrap(m, SyntaxKind::Value);
}

fn simple_value(p: &mut Parser) {
    let m = p.marker();
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
        _ => p.error_and_eat("unexpected"),
    }
    p.wrap(m, SyntaxKind::SimpleValue);
}

fn integer(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::IntVal);
    p.wrap(m, SyntaxKind::Integer);
}

fn string(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::StrVal);
    p.wrap(m, SyntaxKind::String);
}

fn code(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::CodeFragment);
    p.wrap(m, SyntaxKind::Code);
}

fn boolean(p: &mut Parser) {
    let m = p.marker();
    if !(p.eat_if(T![true]) || p.eat_if(T![false])) {
        p.error_and_eat("expected true or false");
    }
    p.wrap(m, SyntaxKind::Boolean);
}

fn uninitialized(p: &mut Parser) {
    let m = p.marker();
    p.expect(T![?]);
    p.wrap(m, SyntaxKind::Uninitialized);
}

fn bits(p: &mut Parser) {
    let m = p.marker();
    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::Bits);
}

fn list(p: &mut Parser) {
    let m = p.marker();
    p.expect(T!['[']);
    while !p.eof() && !p.at(T![']']) {
        value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T![']']);
    p.wrap(m, SyntaxKind::List);
}

fn dag(p: &mut Parser) {
    let m = p.marker();
    p.expect(T!['(']);
    dagarg(p);

    if p.eat_if(T![')']) {
        p.wrap(m, SyntaxKind::Dag);
        return;
    }

    while !p.eof() && !p.at(T![')']) {
        dagarg(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T![')']);
    p.wrap(m, SyntaxKind::Dag);
}

fn dagarg(p: &mut Parser) {
    let m = p.marker();
    if p.eat_if(TokenKind::VarName) {
        p.wrap(m, SyntaxKind::DagArg);
        return;
    }

    value(p);
    if p.eat_if(T![:]) {
        var_name(p);
    }
    p.wrap(m, SyntaxKind::DagArg);
}

fn var_name(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::VarName);
    p.wrap(m, SyntaxKind::VarName);
}

fn identifier(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::Id);
    p.wrap(m, SyntaxKind::Identifier);
}

fn bang_operator(p: &mut Parser) {
    let m = p.marker();
    if !p.current().is_bang_operator() {
        p.error_and_eat("expected bang operator");
    } else {
        p.eat();
        p.expect(T!['(']);
        while !p.eof() && !p.at(T![')']) {
            value(p);
            if !p.eat_if(T![,]) {
                break;
            }
        }
        p.expect(T![')']);
    }
    p.wrap(m, SyntaxKind::BangOperator);
}

fn cond_operator(p: &mut Parser) {
    let m = p.marker();
    p.expect(T![!cond]);
    p.expect(T!['(']);
    while !p.eof() && !p.at(T![')']) {
        cond_clause(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T![')']);
    p.wrap(m, SyntaxKind::CondOperator);
}

fn cond_clause(p: &mut Parser) {
    let m = p.marker();
    if !p.eat_if(TokenKind::VarName) {
        value(p);
        p.expect(T![:]);
        value(p);
    }

    p.wrap(m, SyntaxKind::CondClause);
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn include() {
        insta::assert_display_snapshot!(parse(r#"include "foo.td""#))
    }

    #[test]
    fn class() {
        insta::assert_display_snapshot!(parse("class Foo<int A, int B = 1>: Bar<A, 2>;"));
    }

    #[test]
    fn class_with_body() {
        insta::assert_display_snapshot!(parse(
            "class Foo<int A> {
                int B;
                int C = A;
            }"
        ))
    }

    #[test]
    fn def() {
        insta::assert_display_snapshot!(parse("def Foo : Bar;"))
    }

    #[test]
    fn r#type() {
        insta::assert_display_snapshot!(parse(
            "class Foo<bit A, int B, string C, dag D, bits<32> E, list<int> F, Bar G>;"
        ))
    }

    #[test]
    fn simple_value() {
        insta::assert_display_snapshot!(parse(
            "class Foo<int A = 1, string B = \"hoge\", code C = [{ true }], bit D = false, int E = ?, bits<2> F = {0, 1}, list<int> G = [1, 2], dag H = (add A:$hoge), int I = A, int J = !add(A, B), int K = !cond(false: 1, true: 2)>;"
        ))
    }
}
