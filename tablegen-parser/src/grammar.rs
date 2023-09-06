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
    while !p.eof() {
        class(p);
    }
    p.wrap(m, SyntaxKind::File);
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

fn template_arg_list(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![<]);
    loop {
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

    loop {
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
    loop {
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
    loop {
        body_item(p);
        if p.eat_if(T!['}']) {
            break;
        }
    }
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
    p.expect(T![int]);
    p.wrap(m, SyntaxKind::Type);
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
        TokenKind::Id => identifier(p),
        _ => unimplemented!(),
    }
    p.wrap(m, SyntaxKind::SimpleValue);
}

fn identifier(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::Id);
    p.wrap(m, SyntaxKind::Identifier);
}

fn integer(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::IntVal);
    p.wrap(m, SyntaxKind::Integer);
}

#[cfg(test)]
mod tests {
    use super::parse;

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
}
