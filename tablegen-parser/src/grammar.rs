pub mod statement;
pub mod r#type;
pub mod value;

use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    parser::Parser,
};

pub fn parse(text: &str) -> SyntaxNode {
    let mut parser = Parser::new(text);
    file(&mut parser);
    parser.finish().into_iter().next().unwrap()
}

// File ::= StatementList
fn file(p: &mut Parser) {
    let m = p.marker();
    p.eat_trivia();
    statement::statement_list(p);
    if !p.eof() {
        p.error("unexpected input at top level");
    }
    p.wrap_all(m, SyntaxKind::File);
}

fn delimited<F>(p: &mut Parser, bra: TokenKind, ket: TokenKind, delim: TokenKind, mut parser: F)
where
    F: FnMut(&mut Parser<'_>),
{
    p.expect(bra);
    while !p.at(ket) && !p.eof() {
        parser(p);

        if !p.eat_if(delim) {
            break;
        }
    }
    p.expect(ket);
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
                let D = A;
            }"
        ))
    }

    #[test]
    fn def() {
        insta::assert_display_snapshot!(parse("def Foo : Bar;"))
    }

    #[test]
    fn r#let() {
        insta::assert_display_snapshot!(parse("let A = 1 in { class Foo; }"))
    }

    #[test]
    fn r#type() {
        insta::assert_display_snapshot!(parse(
            "class Foo<bit A, int B, string C, dag D, bits<32> E, list<int> F, Bar G>;"
        ))
    }

    #[test]
    fn value() {
        insta::assert_display_snapshot!(parse("class Foo<int A = Hoge.Fuga>;"));
    }

    #[test]
    fn simple_value() {
        insta::assert_display_snapshot!(parse(
            "class Foo<int A = 1, string B = \"hoge\", bit D = false, int E = ?, bits<2> F = {0, 1}, list<int> G = [1, 2], dag H = (add A:$hoge), int I = A, int J = !add(A, B), int K = !cond(false: 1, true: 2)> {
                code C = [{ true }];
            }"
        ))
    }
}
