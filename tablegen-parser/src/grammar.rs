pub mod statement;
pub mod r#type;
pub mod value;

use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    parser::Parser,
    T,
};

const RECOVER_TOKENS: [TokenKind; 5] = [T![include], T![class], T![def], T![let], T![;]];

pub fn parse(text: &str) -> SyntaxNode {
    let mut parser = Parser::new(text, &RECOVER_TOKENS);
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
    fn statement() {
        insta::assert_display_snapshot!(parse("a"));
    }

    #[test]
    fn include() {
        insta::assert_display_snapshot!(parse(r#"include "foo.td""#));

        insta::assert_display_snapshot!(parse("include"));
    }

    #[test]
    fn class() {
        insta::assert_display_snapshot!(parse("class Foo<int A, int B = 1>: Bar<A, 2>;"));
        insta::assert_display_snapshot!(parse(
            "class Foo<int A> {
                int B;
                int C = A;
                let D = A;
            }"
        ));
        insta::assert_display_snapshot!(parse("class"));
    }

    #[test]
    fn def() {
        insta::assert_display_snapshot!(parse("def Foo : Bar;"))
    }

    #[test]
    fn r#let() {
        insta::assert_display_snapshot!(parse("let A = 1 in { class Foo; }"));

        insta::assert_display_snapshot!(parse("let A = 1"));
        insta::assert_display_snapshot!(parse("let A = 1 {"));
    }

    #[test]
    fn let_item() {
        insta::assert_display_snapshot!(parse("let"));
        insta::assert_display_snapshot!(parse("let A"));
    }

    #[test]
    fn template_arg_decl() {
        insta::assert_display_snapshot!(parse("class Foo<int"));
    }

    #[test]
    fn class_ref() {
        insta::assert_display_snapshot!(parse("class Foo : Bar<"));
    }

    #[test]
    fn r#type() {
        insta::assert_display_snapshot!(parse(
            "class Foo<bit A, int B, string C, dag D, bits<32> E, list<int> F, Bar G>;"
        ));
    }

    #[test]
    fn body() {
        insta::assert_display_snapshot!(parse("class Foo"));
    }

    #[test]
    fn field_def() {
        insta::assert_display_snapshot!(parse("class Foo { int"));
        insta::assert_display_snapshot!(parse("class Foo { int A"));
    }

    #[test]
    fn field_let() {
        insta::assert_display_snapshot!(parse("class Foo { let"));
        insta::assert_display_snapshot!(parse("class Foo { let A"));
        insta::assert_display_snapshot!(parse("class Foo { let A = 1"));
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
        ));
    }
}
