use std::collections::HashSet;

use ecow::EcoString;
use rowan::GreenNode;
use rowan::ast::AstNode;

use crate::error::SyntaxError;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::preprocessor::PreProcessor;
use crate::syntax_kind::SyntaxKind;

pub mod ast;
pub mod error;
pub mod grammar;
pub mod lexer;
pub mod parser;
pub mod preprocessor;
pub mod syntax_kind;
pub mod token_kind;
pub mod token_stream;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Language {}

impl rowan::Language for Language {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 < SyntaxKind::__LAST as u16);
        unsafe { std::mem::transmute(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Language>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<Language>;
pub type SyntaxToken = rowan::SyntaxToken<Language>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Parse {
    green_node: GreenNode,
    errors: Vec<SyntaxError>,
    macros: HashSet<EcoString>,
}

impl Parse {
    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn source_file(&self) -> Option<ast::SourceFile> {
        ast::SourceFile::cast(self.syntax_node())
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    pub fn macros(&self) -> &HashSet<EcoString> {
        &self.macros
    }
}

pub fn parse(text: &str) -> Parse {
    parse_with_macros(text, HashSet::new())
}

pub fn parse_with_macros(text: &str, initial_macros: HashSet<EcoString>) -> Parse {
    let lexer = Lexer::new(text);
    let preprocessor = PreProcessor::new_with_macros(lexer, initial_macros);
    let mut parser = Parser::new(preprocessor);
    grammar::source_file(&mut parser);
    let (green_node, errors, preprocessor) = parser.finish();
    let macros = preprocessor.finish();
    Parse {
        green_node,
        errors,
        macros,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use ecow::EcoString;

    use super::{parse, parse_with_macros};

    #[test]
    fn parse_with_macros_include_guard() {
        let text = "#ifndef GUARD\n#define GUARD\nclass Foo;\n#endif\n";

        let result1 = parse(text);
        assert!(result1.macros().contains("GUARD"));
        let sf1 = result1.source_file().unwrap();
        let sl1 = sf1.statement_list().unwrap();
        assert!(
            sl1.statements().count() > 0,
            "first parse should include content"
        );

        let mut macros = HashSet::new();
        macros.insert(EcoString::from("GUARD"));
        let result2 = parse_with_macros(text, macros);
        let sf2 = result2.source_file().unwrap();
        let has_stmts = sf2
            .statement_list()
            .map(|sl| sl.statements().count())
            .unwrap_or(0);
        assert_eq!(has_stmts, 0, "second parse should skip guarded content");
    }
}
