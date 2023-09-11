use ecow::EcoString;

use crate::{kind::SyntaxKind, node::SyntaxNode};

pub trait AstNode<'a>: Sized {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self>;

    fn to_untyped(self) -> &'a SyntaxNode;
}

macro_rules! node {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name<'a>(&'a SyntaxNode);

        impl<'a> AstNode<'a> for $name<'a> {
            #[inline]
            fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
                match node.kind() {
                    SyntaxKind::$name => Some(Self(node)),
                    _ => None,
                }
            }

            #[inline]
            fn to_untyped(self) -> &'a SyntaxNode {
                self.0
            }
        }
    };
}

node!(File);

