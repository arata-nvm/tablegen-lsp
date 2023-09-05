use crate::{
    kind::{SyntaxKind, TokenKind},
    lexer::Lexer,
    node::SyntaxNode,
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

#[derive(Debug)]
struct Parser<'a> {
    text: &'a str,
    lexer: Lexer<'a>,
    current: TokenKind,
    current_start: usize,
    nodes: Vec<SyntaxNode>,
}

#[derive(Debug)]
struct Marker(usize);

impl<'a> Parser<'a> {
    fn new(text: &'a str) -> Self {
        let mut lexer = Lexer::new(text);
        let current = lexer.next();
        Self {
            text,
            lexer,
            current,
            current_start: 0,
            nodes: vec![],
        }
    }

    fn finish(self) -> Vec<SyntaxNode> {
        self.nodes
    }

    fn current(&self) -> TokenKind {
        self.current
    }

    fn current_start(&self) -> usize {
        self.current_start
    }

    fn current_end(&self) -> usize {
        self.lexer.cursor()
    }

    fn current_text(&self) -> &'a str {
        &self.text[self.current_start()..self.current_end()]
    }

    fn marker(&self) -> Marker {
        Marker(self.nodes.len())
    }

    fn wrap(&mut self, from: Marker, kind: SyntaxKind) {
        let from = from.0;
        let to = self.nodes.len();
        let children = self.nodes.drain(from..to).collect();
        self.nodes.insert(from, SyntaxNode::node(kind, children));
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current == kind
    }

    fn eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    fn assert(&mut self, kind: TokenKind) {
        assert_eq!(self.current, kind);
        self.eat();
    }

    fn expect(&mut self, kind: TokenKind) {
        if !self.eat_if(kind) {
            unimplemented!("expected {kind:?}");
        }
    }

    fn eat(&mut self) {
        self.consume_token();
        while self.current.is_trivia() {
            self.consume_token();
        }
    }

    fn consume_token(&mut self) {
        let text = self.current_text();
        self.nodes.push(SyntaxNode::token(self.current, text));

        self.current_start = self.lexer.cursor();
        self.current = self.lexer.next();
    }

    fn eat_if(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.eat();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(parse("class Foo<int A, int B = 1>: Bar<A, 2>;"));
    }

    #[test]
    fn class_with_body() {
        insta::assert_debug_snapshot!(parse(
            "class Foo<int A> {
                int B;
                int C = A;
            }"
        ))
    }
}
