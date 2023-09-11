use ecow::EcoString;

use crate::{kind::SyntaxKind, node::SyntaxNode, T};

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

impl<'a> File<'a> {
    pub fn classes(self) -> impl DoubleEndedIterator<Item = Class<'a>> {
        self.0.children().filter_map(|node| node.cast())
    }
}

node!(Include);

impl<'a> Include<'a> {
    pub fn path(self) -> Option<String<'a>> {
        self.0.cast_first_match()
    }
}

node!(Class);

impl<'a> Class<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn template_args(self) -> Option<TemplateArgList<'a>> {
        self.0.cast_first_match()
    }

    pub fn record_body(self) -> Option<RecordBody<'a>> {
        self.0.cast_first_match()
    }
}

node!(TemplateArgList);

impl<'a> TemplateArgList<'a> {
    pub fn args(self) -> impl DoubleEndedIterator<Item = TemplateArgDecl<'a>> {
        self.0.children().filter_map(|node| node.cast())
    }
}

node!(TemplateArgDecl);

impl<'a> TemplateArgDecl<'a> {
    pub fn r#type(self) -> Option<Type<'a>> {
        self.0.cast_first_match()
    }

    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }
}

node!(RecordBody);

impl<'a> RecordBody<'a> {
    pub fn parent_classes(self) -> Option<ParentClassList<'a>> {
        self.0.cast_first_match()
    }

    pub fn body(self) -> Option<Body<'a>> {
        self.0.cast_first_match()
    }
}

node!(ParentClassList);

impl<'a> ParentClassList<'a> {
    pub fn classes(self) -> impl DoubleEndedIterator<Item = ClassRef<'a>> {
        self.0.children().filter_map(|node| node.cast())
    }
}

node!(ClassRef);

impl<'a> ClassRef<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn args(self) -> Option<ArgValueList<'a>> {
        self.0.cast_first_match()
    }
}

node!(ArgValueList);

impl<'a> ArgValueList<'a> {
    pub fn positional_args(self) -> Option<PositionalArgValueList<'a>> {
        self.0.cast_first_match()
    }
}

node!(PositionalArgValueList);

impl<'a> PositionalArgValueList<'a> {
    pub fn args(self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        self.0.children().filter_map(|node| node.cast())
    }
}

node!(Body);

impl<'a> Body<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = BodyItem<'a>> {
        self.0.children().filter_map(|node| node.cast())
    }
}

node!(BodyItem);

impl<'a> BodyItem<'a> {
    pub fn r#type(self) -> Option<Type<'a>> {
        self.0.cast_first_match()
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }
}

#[derive(Debug)]
pub enum Type<'a> {
    Bit(&'a SyntaxNode),
    Int(&'a SyntaxNode),
    String(&'a SyntaxNode),
    Dag(&'a SyntaxNode),
    Bits(BitsType<'a>),
    List(ListType<'a>),
    ClassRef(ClassRef<'a>),
}

impl<'a> AstNode<'a> for Type<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.token_kind() {
            T![bit] => return Some(Self::Bit(node)),
            T![int] => return Some(Self::Int(node)),
            T![string] => return Some(Self::String(node)),
            T![dag] => return Some(Self::Dag(node)),
            _ => {}
        }

        match node.kind() {
            SyntaxKind::BitsType => node.cast().map(Self::Bits),
            SyntaxKind::ListType => node.cast().map(Self::List),
            SyntaxKind::ClassRef => node.cast().map(Self::ClassRef),
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Bit(n) | Self::Int(n) | Self::String(n) | Self::Dag(n) => n,
            Self::Bits(v) => v.to_untyped(),
            Self::List(v) => v.to_untyped(),
            Self::ClassRef(v) => v.to_untyped(),
        }
    }
}

node!(BitsType);

impl<'a> BitsType<'a> {
    pub fn length(self) -> Option<Integer<'a>> {
        self.0.cast_first_match()
    }
}

node!(ListType);

impl<'a> ListType<'a> {
    pub fn inner_type(self) -> Option<Type<'a>> {
        self.0.cast_first_match()
    }
}

#[derive(Debug)]
pub enum Value<'a> {
    SimpleValue(SimpleValue<'a>),
}

impl<'a> AstNode<'a> for Value<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::SimpleValue => node.cast().map(Self::SimpleValue),
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::SimpleValue(node) => node.to_untyped(),
        }
    }
}

#[derive(Debug)]
pub enum SimpleValue<'a> {
    Identifier(Identifier<'a>),
    Integer(Integer<'a>),
    String(String<'a>),
}

impl<'a> AstNode<'a> for SimpleValue<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Identifier => node.cast().map(Self::Identifier),
            SyntaxKind::Integer => node.cast().map(Self::Identifier),
            SyntaxKind::String => node.cast().map(Self::String),
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Identifier(node) => node.to_untyped(),
            Self::Integer(node) => node.to_untyped(),
            Self::String(node) => node.to_untyped(),
        }
    }
}

node!(Identifier);

impl<'a> Identifier<'a> {
    pub fn value(self) -> Option<&'a EcoString> {
        self.0.children().next().map(|node| node.text())
    }
}

node!(Integer);

impl Integer<'_> {
    pub fn value(self) -> Option<i64> {
        let text = self.0.text();
        if let Some(rest) = text.strip_prefix("0x") {
            i64::from_str_radix(rest, 16).ok()
        } else {
            i64::from_str_radix(text, 10).ok()
        }
    }
}

node!(String);

impl<'a> String<'a> {
    pub fn value(self) -> Option<&'a EcoString> {
        self.0.children().next().map(|node| node.text())
    }
}
