pub mod statement;
pub mod r#type;
pub mod value;

use crate::{
    error::SyntaxError, grammar::statement::StatementListType, language::SyntaxNode,
    parser::Parser, syntax_kind::SyntaxKind, token_kind::TokenKind, T,
};

const RECOVER_TOKENS: [TokenKind; 5] = [T![include], T![class], T![def], T![let], T![;]];

pub fn parse(text: &str) -> (SyntaxNode, Vec<SyntaxError>) {
    let mut parser = Parser::new(text, &RECOVER_TOKENS);
    root(&mut parser);

    let errors = parser.take_errors();
    (parser.finish(), errors)
}

// Root ::= StatementList
fn root(p: &mut Parser) {
    p.start_node(SyntaxKind::Root);
    statement::statement_list(p, StatementListType::TopLevel);
    if !p.eof() {
        p.error("unexpected input at top level");
    }
    p.finish_node();
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
    use crate::grammar::parse;

    #[test]
    fn statement() {
        insta::assert_debug_snapshot!(parse("a"));
    }

    #[test]
    fn include() {
        insta::assert_debug_snapshot!(parse(r#"include "foo.td""#));

        insta::assert_debug_snapshot!(parse("include"));
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(parse("class Foo<int A, int B = 1>: Bar<A, 2>;"));
        insta::assert_debug_snapshot!(parse(
            "class Foo<int A> {
                int B;
                int C = A;
                let D = A;
            }"
        ));
        insta::assert_debug_snapshot!(parse("class"));
    }

    #[test]
    fn def() {
        insta::assert_debug_snapshot!(parse("def Foo : Bar;"));
        insta::assert_debug_snapshot!(parse("def foo {}"));
    }

    #[test]
    fn r#let() {
        insta::assert_debug_snapshot!(parse("let A = 1, B<1...3> = 0b101 in { class Foo; }"));

        insta::assert_debug_snapshot!(parse("let A = 1"));
        insta::assert_debug_snapshot!(parse("let A = 1 {"));
    }

    #[test]
    fn let_item() {
        insta::assert_debug_snapshot!(parse("let"));
        insta::assert_debug_snapshot!(parse("let A"));
    }

    #[test]
    fn multi_class() {
        insta::assert_debug_snapshot!(parse(
            "multiclass foo {
                def _foo1;
                def _foo2;
            }"
        ));
    }

    #[test]
    fn defm() {
        insta::assert_debug_snapshot!(parse("defm foo : bar;"));
    }

    #[test]
    fn defset() {
        insta::assert_debug_snapshot!(parse(
            "defset list<Base> BaseList = {
                def Foo0 : Base<0>;
                def Foo1 : Base<1>;
            }"
        ));
    }

    #[test]
    fn defvar() {
        insta::assert_debug_snapshot!(parse("defvar i = 0;"));
    }

    #[test]
    fn foreach() {
        insta::assert_debug_snapshot!(parse(
            "foreach i = [0, 1] in {
                def Foo # i : Base<i>;
            }"
        ));
    }

    #[test]
    fn r#if() {
        insta::assert_debug_snapshot!(parse("if true then { class Foo; }"));
    }

    #[test]
    fn assert() {
        insta::assert_debug_snapshot!(parse("assert hoge, \"fuga\";"));
    }

    #[test]
    fn template_arg_decl() {
        insta::assert_debug_snapshot!(parse("class Foo<int"));
    }

    #[test]
    fn class_ref() {
        insta::assert_debug_snapshot!(parse("class Foo : Bar<"));
    }

    #[test]
    fn r#type() {
        insta::assert_debug_snapshot!(parse(
            "class Foo<bit A, int B, string C, dag D, bits<32> E, list<int> F, Bar G>;"
        ));
    }

    #[test]
    fn body() {
        insta::assert_debug_snapshot!(parse("class Foo"));
    }

    #[test]
    fn field_def() {
        insta::assert_debug_snapshot!(parse("class Foo { int"));
        insta::assert_debug_snapshot!(parse("class Foo { int A"));
    }

    #[test]
    fn field_let() {
        insta::assert_debug_snapshot!(parse("class Foo { let"));
        insta::assert_debug_snapshot!(parse("class Foo { let A"));
        insta::assert_debug_snapshot!(parse("class Foo { let A = 1"));
    }

    #[test]
    fn value() {
        insta::assert_debug_snapshot!(parse("class Foo<string A = \"hoge\" # \"fuga\">;"));
    }

    #[test]
    fn inner_value() {
        insta::assert_debug_snapshot!(parse(
            "class Foo<int A = Hoge.Fuga, bits<2> B = Hoge{0...1}, list<int> C = Hoge[0...1]>;"
        ));
    }

    #[test]
    fn simple_value() {
        insta::assert_debug_snapshot!(parse(
            "class Foo<int A = 1, string B = \"hoge\", bit D = false, int E = ?, bits<2> F = {0, 1}, list<int> G = [1, 2], dag H = (add A:$hoge), int I = A, int J = !add(A, B), int K = !cond(false: 1, true: 2)> {
                code C = [{ true }];
            }"
        ));
    }
}
