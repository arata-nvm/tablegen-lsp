use ecow::EcoString;

use crate::kind::{SyntaxKind, TokenKind};

#[derive(Debug)]
pub struct SyntaxNode(SyntaxNodeInner);

#[derive(Debug)]
enum SyntaxNodeInner {
    Token(TokenKind, EcoString),
    Node(SyntaxKind, Vec<SyntaxNode>),
}

impl SyntaxNode {
    pub fn token(kind: TokenKind, text: impl Into<EcoString>) -> Self {
        Self(SyntaxNodeInner::Token(kind, text.into()))
    }

    pub fn node(kind: SyntaxKind, children: Vec<SyntaxNode>) -> Self {
        Self(SyntaxNodeInner::Node(kind, children))
    }

    pub fn kind(&self) -> SyntaxKind {
        match self.0 {
            SyntaxNodeInner::Token(kind, _) => todo!(),
            SyntaxNodeInner::Node(kind, _) => kind,
        }
    }
}
