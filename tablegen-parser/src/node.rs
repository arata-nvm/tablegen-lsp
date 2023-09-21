use core::fmt;

use ecow::EcoString;

use crate::{
    ast::AstNode,
    error::{Span, SyntaxError},
    kind::{SyntaxKind, TokenKind},
};

#[derive(Debug)]
pub struct SyntaxNode(SyntaxNodeInner);

#[derive(Debug)]
enum SyntaxNodeInner {
    Token(TokenKind, EcoString, Span),
    Node(SyntaxKind, Vec<SyntaxNode>),
    Error(SyntaxError, EcoString),
}

impl SyntaxNode {
    pub fn token(kind: TokenKind, text: impl Into<EcoString>, span: Span) -> Self {
        Self(SyntaxNodeInner::Token(kind, text.into(), span))
    }

    pub fn node(kind: SyntaxKind, children: Vec<SyntaxNode>) -> Self {
        Self(SyntaxNodeInner::Node(kind, children))
    }

    pub fn error(span: Span, message: impl Into<EcoString>, text: impl Into<EcoString>) -> Self {
        Self(SyntaxNodeInner::Error(
            SyntaxError::new(span, message),
            text.into(),
        ))
    }

    pub fn kind(&self) -> SyntaxKind {
        match self.0 {
            SyntaxNodeInner::Token(_, _, _) => SyntaxKind::Token,
            SyntaxNodeInner::Node(kind, _) => kind,
            SyntaxNodeInner::Error(_, _) => SyntaxKind::Error,
        }
    }

    pub fn token_kind(&self) -> TokenKind {
        match self.0 {
            SyntaxNodeInner::Token(kind, _, _) => kind,
            SyntaxNodeInner::Node(_, _) => TokenKind::Error,
            SyntaxNodeInner::Error(_, _) => TokenKind::Error,
        }
    }

    pub fn text(&self) -> &EcoString {
        static EMPTY: EcoString = EcoString::new();
        match self.0 {
            SyntaxNodeInner::Token(_, ref text, _) => text,
            SyntaxNodeInner::Node(_, _) => &EMPTY,
            SyntaxNodeInner::Error(_, ref text) => text,
        }
    }

    pub fn span(&self) -> Span {
        match self.0 {
            SyntaxNodeInner::Token(_, _, ref span) => span.clone(),
            SyntaxNodeInner::Node(_, ref children) => {
                let start = children
                    .first()
                    .map(|node| node.span().start)
                    .unwrap_or_default();
                let end = children
                    .last()
                    .map(|node| node.span().end)
                    .unwrap_or_default();
                start..end
            }
            SyntaxNodeInner::Error(ref error, _) => error.span.clone(),
        }
    }

    pub fn children(&self) -> std::slice::Iter<'_, SyntaxNode> {
        match self.0 {
            SyntaxNodeInner::Token(_, _, _) | SyntaxNodeInner::Error(_, _) => [].iter(),
            SyntaxNodeInner::Node(_, ref children) => children.iter(),
        }
    }

    pub fn cast<'a, T: AstNode<'a>>(&'a self) -> Option<T> {
        T::from_untyped(self)
    }

    pub fn cast_first_match<'a, T: AstNode<'a>>(&'a self) -> Option<T> {
        self.children().find_map(Self::cast)
    }

    pub fn cast_all_matches<'a, T: AstNode<'a>>(
        &'a self,
    ) -> impl DoubleEndedIterator<Item = T> + 'a {
        self.children().filter_map(|node| node.cast())
    }

    pub fn first_child_text<'a>(&'a self) -> Option<&'a EcoString> {
        self.children().next().map(|node| node.text())
    }

    pub fn errors(&self) -> Vec<&SyntaxError> {
        match &self.0 {
            SyntaxNodeInner::Error(error, _) => vec![error],
            _ => self.children().flat_map(|node| node.errors()).collect(),
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
        SyntaxNodeInner::Token(kind, text, span) => {
            writeln!(f, "{span:?} {kind:?} `{}`", text.escape_default())
        }
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
