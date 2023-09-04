use crate::{kind::SyntaxKind, lexer::Lexer, node::SyntaxNode};

pub fn parse(text: &str) -> SyntaxNode {
    let mut parser = Parser::new(text);
    class(&mut parser);
    parser.finish().into_iter().next().unwrap()
}

fn class(p: &mut Parser) {
    let m = p.marker();
    p.assert(SyntaxKind::Class);
    p.expect(SyntaxKind::Id);
    p.expect(SyntaxKind::Semi);
    p.wrap(m, SyntaxKind::ClassDef);
}

#[derive(Debug)]
struct Parser<'a> {
    text: &'a str,
    lexer: Lexer<'a>,
    current: SyntaxKind,
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

    fn assert(&mut self, kind: SyntaxKind) {
        assert_eq!(self.current, kind);
        self.eat();
    }

    fn expect(&mut self, kind: SyntaxKind) {
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

    fn eat_if(&mut self, kind: SyntaxKind) -> bool {
        if self.current == kind {
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
        insta::assert_debug_snapshot!(parse("class Foo;"));
    }
}
