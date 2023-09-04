use crate::{
    kind::{SyntaxKind, TokenKind},
    lexer::Lexer,
    node::SyntaxNode,
    T,
};

pub fn parse(text: &str) -> SyntaxNode {
    let mut parser = Parser::new(text);
    class(&mut parser);
    parser.finish().into_iter().next().unwrap()
}

fn class(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![class]);
    identifier(p);
    if p.at(T![<]) {
        template_arg_list(p);
    }
    p.expect(T![;]);
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
    p.wrap(m, SyntaxKind::TemplateArgDecl);
}

fn r#type(p: &mut Parser) {
    let m = p.marker();
    p.expect(T![int]);
    p.wrap(m, SyntaxKind::Type);
}

fn identifier(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::Id);
    p.wrap(m, SyntaxKind::Identifier);
}

#[derive(Debug)]
struct Parser<'a> {
    text: &'a str,
    lexer: Lexer<'a>,
    current: TokenKind,
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
            nodes: vec![],
        }
    }

    fn finish(self) -> Vec<SyntaxNode> {
        self.nodes
    }

    fn marker(&self) -> Marker {
        Marker(self.nodes.len())
    }

    fn wrap(&mut self, from: Marker, kind: SyntaxKind) {
        let from = from.0;
        let to = self.nodes.len();
        let children = self.nodes.drain(from..to).collect();
        self.nodes.insert(from, SyntaxNode(kind, children));
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current == kind
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
        self.current = self.lexer.next();
        while self.current.is_trivia() {
            self.current = self.lexer.next();
        }
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
        insta::assert_debug_snapshot!(parse("class Foo<int A, int B>;"));
    }
}
