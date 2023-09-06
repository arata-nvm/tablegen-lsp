use core::fmt;

use ecow::EcoString;

use crate::kind::{SyntaxKind, TokenKind};

#[derive(Debug)]
pub struct SyntaxNode(SyntaxNodeInner);

#[derive(Debug)]
enum SyntaxNodeInner {
    Token(TokenKind, EcoString),
    Node(SyntaxKind, Vec<SyntaxNode>),
    Error(EcoString, EcoString),
}

impl SyntaxNode {
    pub fn token(kind: TokenKind, text: impl Into<EcoString>) -> Self {
        Self(SyntaxNodeInner::Token(kind, text.into()))
    }

    pub fn node(kind: SyntaxKind, children: Vec<SyntaxNode>) -> Self {
        Self(SyntaxNodeInner::Node(kind, children))
    }

    pub fn error(message: impl Into<EcoString>, text: impl Into<EcoString>) -> Self {
        Self(SyntaxNodeInner::Error(message.into(), text.into()))
    }

    pub fn kind(&self) -> SyntaxKind {
        match self.0 {
            SyntaxNodeInner::Token(_, _) => todo!(),
            SyntaxNodeInner::Node(kind, _) => kind,
            SyntaxNodeInner::Error(_, _) => SyntaxKind::Error,
        }
    }
}

impl fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        dump(self, 0, f)
    }
}

fn dump(node: &SyntaxNode, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", "  ".repeat(depth))?;

    match &node.0 {
        SyntaxNodeInner::Token(kind, text) => writeln!(f, "{kind:?} `{}`", text.escape_default()),
        SyntaxNodeInner::Node(kind, children) => {
            writeln!(f, "{kind:?}")?;
            for child in children {
                dump(child, depth + 1, f)?;
            }
            Ok(())
        }
        SyntaxNodeInner::Error(message, text) => writeln!(f, "Error({message}, {text})"),
    }
}
