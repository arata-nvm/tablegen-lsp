use ecow::EcoString;

use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    T,
};

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
    pub fn classes(self) -> impl DoubleEndedIterator<Item = FileItem<'a>> {
        self.0.cast_all_matches()
    }
}

#[derive(Debug)]
pub enum FileItem<'a> {
    Include(Include<'a>),
    Class(Class<'a>),
    Def(Def<'a>),
}

impl<'a> AstNode<'a> for FileItem<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Include => node.cast().map(Self::Include),
            SyntaxKind::Class => node.cast().map(Self::Class),
            SyntaxKind::Def => node.cast().map(Self::Def),
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Include(v) => v.to_untyped(),
            Self::Class(v) => v.to_untyped(),
            Self::Def(v) => v.to_untyped(),
        }
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

node!(Def);

impl<'a> Def<'a> {
    pub fn name(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn record_body(self) -> Option<RecordBody<'a>> {
        self.0.cast_first_match()
    }
}

node!(TemplateArgList);

impl<'a> TemplateArgList<'a> {
    pub fn args(self) -> impl DoubleEndedIterator<Item = TemplateArgDecl<'a>> {
        self.0.cast_all_matches()
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
        self.0.cast_all_matches()
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
        self.0.cast_all_matches()
    }
}

node!(Body);

impl<'a> Body<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = BodyItem<'a>> {
        self.0.cast_all_matches()
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
    Code(&'a SyntaxNode),
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
            T![code] => return Some(Self::Code(node)),
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
            Self::Bit(n) | Self::Int(n) | Self::String(n) | Self::Dag(n) | Self::Code(n) => n,
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

node!(Value);

impl<'a> Value<'a> {
    pub fn simple_value(self) -> Option<SimpleValue<'a>> {
        self.0.cast_first_match()
    }

    pub fn suffixes(self) -> impl DoubleEndedIterator<Item = ValueSuffix<'a>> {
        self.0.cast_all_matches()
    }
}

#[derive(Debug)]
pub enum ValueSuffix<'a> {
    Field(Field<'a>),
}

impl<'a> AstNode<'a> for ValueSuffix<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Field => node.cast().map(Self::Field),
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Field(v) => v.to_untyped(),
        }
    }
}

node!(Field);

impl<'a> Field<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }
}

#[derive(Debug)]
pub enum SimpleValue<'a> {
    Integer(Integer<'a>),
    String(String<'a>),
    Code(Code<'a>),
    Boolean(Boolean<'a>),
    Uninitialized(Uninitialized<'a>),
    Bits(Bits<'a>),
    List(List<'a>),
    Dag(Dag<'a>),
    Identifier(Identifier<'a>),
    ClassRef(ClassRef<'a>),
    BangOperator(BangOperator<'a>),
    CondOperator(CondOperator<'a>),
}

impl<'a> AstNode<'a> for SimpleValue<'a> {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::Integer => node.cast().map(Self::Identifier),
            SyntaxKind::String => node.cast().map(Self::String),
            SyntaxKind::Code => node.cast().map(Self::Code),
            SyntaxKind::Boolean => node.cast().map(Self::Boolean),
            SyntaxKind::Uninitialized => node.cast().map(Self::Uninitialized),
            SyntaxKind::Bits => node.cast().map(Self::Bits),
            SyntaxKind::List => node.cast().map(Self::List),
            SyntaxKind::Dag => node.cast().map(Self::Dag),
            SyntaxKind::Identifier => node.cast().map(Self::Identifier),
            SyntaxKind::ClassRef => node.cast().map(Self::ClassRef),
            SyntaxKind::BangOperator => node.cast().map(Self::BangOperator),
            SyntaxKind::CondOperator => node.cast().map(Self::CondOperator),
            _ => None,
        }
    }

    fn to_untyped(self) -> &'a SyntaxNode {
        match self {
            Self::Integer(node) => node.to_untyped(),
            Self::String(node) => node.to_untyped(),
            Self::Code(node) => node.to_untyped(),
            Self::Boolean(node) => node.to_untyped(),
            Self::Uninitialized(node) => node.to_untyped(),
            Self::Bits(node) => node.to_untyped(),
            Self::List(node) => node.to_untyped(),
            Self::Dag(node) => node.to_untyped(),
            Self::Identifier(node) => node.to_untyped(),
            Self::ClassRef(node) => node.to_untyped(),
            Self::BangOperator(node) => node.to_untyped(),
            Self::CondOperator(node) => node.to_untyped(),
        }
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
        self.0.first_child_text()
    }
}

node!(Code);

impl<'a> Code<'a> {
    pub fn value(self) -> Option<EcoString> {
        self.0.children().next().map(|node| {
            node.text()
                .trim_start_matches("[{")
                .trim_end_matches("}]")
                .trim()
                .into()
        })
    }
}

node!(Boolean);

impl<'a> Boolean<'a> {
    pub fn value(self) -> Option<bool> {
        let text = self.0.text();
        match text.as_str() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        }
    }
}

node!(Uninitialized);

node!(Bits);

impl<'a> Bits<'a> {
    pub fn values(self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        self.0.cast_all_matches()
    }
}

node!(List);

impl<'a> List<'a> {
    pub fn values(self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        self.0.cast_all_matches()
    }
}

node!(Dag);

impl<'a> Dag<'a> {
    pub fn args(self) -> impl DoubleEndedIterator<Item = DagArg<'a>> {
        self.0.cast_all_matches()
    }
}

node!(DagArg);

impl<'a> DagArg<'a> {
    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn var_name(self) -> Option<VarName<'a>> {
        self.0.cast_first_match()
    }
}

node!(VarName);

impl<'a> VarName<'a> {
    pub fn value(self) -> Option<&'a EcoString> {
        self.0.first_child_text()
    }
}

node!(Identifier);

impl<'a> Identifier<'a> {
    pub fn value(self) -> Option<&'a EcoString> {
        self.0.first_child_text()
    }
}

node!(BangOperator);

impl<'a> BangOperator<'a> {
    pub fn kind(self) -> Option<TokenKind> {
        self.0.children().next().map(|node| node.token_kind())
    }

    pub fn values(self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        self.0.cast_all_matches()
    }
}

node!(CondOperator);

impl<'a> CondOperator<'a> {
    pub fn clauses(self) -> impl DoubleEndedIterator<Item = CondClause<'a>> {
        self.0.cast_all_matches()
    }
}

node!(CondClause);

impl<'a> CondClause<'a> {
    pub fn condition(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(1)
    }
}
