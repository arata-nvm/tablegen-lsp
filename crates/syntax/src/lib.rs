use rowan::ast::AstNode;

use crate::error::SyntaxError;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::preprocessor::PreProcessor;
use crate::syntax_kind::SyntaxKind;
use crate::token_kind::TokenKind;

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
pub type SyntaxToken = rowan::SyntaxToken<Language>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

#[derive(Debug, Clone)]
pub struct Parse {
    root_node: SyntaxNode,
    errors: Vec<SyntaxError>,
}

impl Parse {
    pub fn root_node(&self) -> SyntaxNode {
        self.root_node.clone()
    }

    pub fn root(&self) -> Option<ast::Root> {
        ast::Root::cast(self.root_node.clone())
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

const RECOVER_TOKENS: [TokenKind; 5] = [T![include], T![class], T![def], T![let], T![;]];

pub fn parse(text: &str) -> Parse {
    let lexer = Lexer::new(text);
    let preprocessor = PreProcessor::new(lexer);
    let mut parser = Parser::new(preprocessor, &RECOVER_TOKENS);
    grammar::root(&mut parser);
    let (root_node, errors) = parser.finish();
    Parse { root_node, errors }
}
