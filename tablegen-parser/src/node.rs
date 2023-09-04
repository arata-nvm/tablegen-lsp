use crate::kind::SyntaxKind;

#[derive(Debug)]
pub struct SyntaxNode(pub SyntaxKind, pub Vec<SyntaxNode>);
