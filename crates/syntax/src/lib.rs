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
}

pub fn parse(text: &str) -> Parse {
    let lexer = Lexer::new(text);
    let preprocessor = PreProcessor::new(lexer);
    let mut parser = Parser::new(preprocessor);
    grammar::source_file(&mut parser);
    let (green_node, errors) = parser.finish();
    Parse { green_node, errors }
}
