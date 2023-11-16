use ecow::EcoString;

use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    parser::Range,
};

pub trait AstNode<'a>: Sized {
    fn from_untyped(node: &'a SyntaxNode) -> Option<Self>;

    fn to_untyped(self) -> &'a SyntaxNode;
}

macro_rules! node {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name<'a>(&'a SyntaxNode);

        impl<'a> $name<'a> {
            pub fn range(&self) -> Range {
                self.0.range()
            }
        }

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

macro_rules! node_enum {
    ($name:ident, [$($item:ident),*]) => {
        #[derive(Debug)]
        pub enum $name<'a> {
            $($item($item<'a>),)*
        }

        impl<'a> AstNode<'a> for $name<'a> {
            #[inline]
            fn from_untyped(node: &'a SyntaxNode) -> Option<Self> {
                match node.kind() {
                    $(SyntaxKind::$item => node.cast().map(Self::$item),)*
                    _ => None,
                }
            }

            #[inline]
            fn to_untyped(self) -> &'a SyntaxNode {
                match self {
                    $(Self::$item(v) => v.to_untyped(),)*
                }
            }
        }
    };
}

node!(File);

impl<'a> File<'a> {
    pub fn statement_list(self) -> Option<StatementList<'a>> {
        self.0.cast_first_match()
    }
}

node!(StatementList);

impl<'a> StatementList<'a> {
    pub fn statements(self) -> impl DoubleEndedIterator<Item = Statement<'a>> {
        self.0.cast_all_matches()
    }
}

node_enum!(Statement, [Include, Class, Def, Let]);

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

    pub fn template_arg_list(self) -> Option<TemplateArgList<'a>> {
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

node!(Let);

impl<'a> Let<'a> {
    pub fn let_list(self) -> Option<LetList<'a>> {
        self.0.cast_first_match()
    }

    pub fn body(self) -> impl DoubleEndedIterator<Item = Statement<'a>> {
        self.0.cast_all_matches()
    }
}

node!(LetList);

impl<'a> LetList<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = LetItem<'a>> {
        self.0.cast_all_matches()
    }
}

node!(LetItem);

impl<'a> LetItem<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn range_list(self) -> Option<RangeList<'a>> {
        self.0.cast_first_match()
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }
}

node!(MultiClass);

impl<'a> MultiClass<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn template_arg_list(self) -> Option<TemplateArgList<'a>> {
        self.0.cast_first_match()
    }

    pub fn parent_class_list(self) -> Option<ParentClassList<'a>> {
        self.0.cast_first_match()
    }

    pub fn statement_list(self) -> Option<StatementList<'a>> {
        self.0.cast_first_match()
    }
}

node!(Defm);

impl<'a> Defm<'a> {
    pub fn name(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn parent_class_list(self) -> Option<ParentClassList<'a>> {
        self.0.cast_first_match()
    }
}

node!(Defset);

impl<'a> Defset<'a> {
    pub fn r#type(self) -> Option<Type<'a>> {
        self.0.cast_first_match()
    }

    pub fn name(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn statement_list(self) -> Option<StatementList<'a>> {
        self.0.cast_first_match()
    }
}

node!(Defvar);

impl<'a> Defvar<'a> {
    pub fn name(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }
}

node!(Foreach);

impl<'a> Foreach<'a> {
    pub fn iterator(self) -> Option<ForeachIterator<'a>> {
        self.0.cast_first_match()
    }

    pub fn body(self) -> Option<StatementList<'a>> {
        self.0.cast_first_match()
    }
}

node!(ForeachIterator);

impl<'a> ForeachIterator<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn init(self) -> Option<ForeachIteratorInit<'a>> {
        self.0.cast_first_match()
    }
}

node_enum!(ForeachIteratorInit, [RangeList, RangePiece, Value]);

node!(If);

impl<'a> If<'a> {
    pub fn condition(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn statement_list(self) -> Option<StatementList<'a>> {
        self.0.cast_first_match()
    }
}

node!(Assert);

impl<'a> Assert<'a> {
    pub fn condition(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }

    pub fn message(self) -> Option<Value<'a>> {
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
    pub fn parent_class_list(self) -> Option<ParentClassList<'a>> {
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

    pub fn arg_value_list(self) -> Option<ArgValueList<'a>> {
        self.0.cast_first_match()
    }
}

node!(ArgValueList);

impl<'a> ArgValueList<'a> {
    pub fn positional(self) -> Option<PositionalArgValueList<'a>> {
        self.0.cast_first_match()
    }

    pub fn named(self) -> Option<NamedArgValueList<'a>> {
        self.0.cast_first_match()
    }
}

node!(PositionalArgValueList);

impl<'a> PositionalArgValueList<'a> {
    pub fn values(self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        self.0.cast_all_matches()
    }
}

node!(NamedArgValueList);

impl<'a> NamedArgValueList<'a> {
    pub fn values(self) -> impl DoubleEndedIterator<Item = NamedArgValue<'a>> {
        self.0.cast_all_matches()
    }
}

node!(NamedArgValue);

impl<'a> NamedArgValue<'a> {
    pub fn name(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(1)
    }
}

node!(Body);

impl<'a> Body<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = BodyItem<'a>> {
        self.0.cast_all_matches()
    }
}

node_enum!(BodyItem, [FieldDef, FieldLet, Defvar, Assert]);

node!(FieldDef);

impl<'a> FieldDef<'a> {
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

node!(CodeType);

node!(FieldLet);

impl<'a> FieldLet<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }

    pub fn value(self) -> Option<Value<'a>> {
        self.0.cast_first_match()
    }
}

node_enum!(
    Type,
    [BitType, IntType, StringType, DagType, BitsType, ListType, ClassId, CodeType]
);

node!(BitType);
node!(IntType);
node!(StringType);
node!(DagType);

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

node!(ClassId);

impl<'a> ClassId<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }
}

node!(Value);

impl<'a> Value<'a> {
    pub fn inner_values(self) -> impl DoubleEndedIterator<Item = InnerValue<'a>> {
        self.0.cast_all_matches()
    }
}

node!(InnerValue);

impl<'a> InnerValue<'a> {
    pub fn simple_value(self) -> Option<SimpleValue<'a>> {
        self.0.cast_first_match()
    }

    pub fn suffixes(self) -> impl DoubleEndedIterator<Item = ValueSuffix<'a>> {
        self.0.cast_all_matches()
    }
}

node_enum!(ValueSuffix, [RangeSuffix, SliceSuffix, FieldSuffix]);

node!(RangeSuffix);

impl<'a> RangeSuffix<'a> {
    pub fn range_list(self) -> Option<RangeList<'a>> {
        self.0.cast_first_match()
    }
}

node!(RangeList);

impl<'a> RangeList<'a> {
    pub fn pieces(self) -> impl DoubleEndedIterator<Item = RangePiece<'a>> {
        self.0.cast_all_matches()
    }
}

node!(RangePiece);

impl<'a> RangePiece<'a> {
    pub fn start(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn end(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(1)
    }
}

node!(SliceSuffix);

impl<'a> SliceSuffix<'a> {
    pub fn element_list(self) -> Option<SliceElements<'a>> {
        self.0.cast_first_match()
    }
}

node!(SliceElements);

impl<'a> SliceElements<'a> {
    pub fn elements(self) -> impl DoubleEndedIterator<Item = SliceElement<'a>> {
        self.0.cast_all_matches()
    }
}

node!(SliceElement);

impl<'a> SliceElement<'a> {
    pub fn start(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn end(self) -> Option<Value<'a>> {
        self.0.cast_all_matches().nth(1)
    }
}

node!(FieldSuffix);

impl<'a> FieldSuffix<'a> {
    pub fn name(self) -> Option<Identifier<'a>> {
        self.0.cast_first_match()
    }
}

node_enum!(
    SimpleValue,
    [
        Integer,
        String,
        Code,
        Boolean,
        Uninitialized,
        Bits,
        List,
        Dag,
        Identifier,
        ClassRef,
        BangOperator,
        CondOperator
    ]
);

node!(Integer);

impl Integer<'_> {
    pub fn value(self) -> Option<i64> {
        let text = self.0.children().next()?.text();
        if let Some(rest) = text.strip_prefix("0x") {
            i64::from_str_radix(rest, 16).ok()
        } else {
            i64::from_str_radix(text, 10).ok()
        }
    }
}

node!(String);

impl<'a> String<'a> {
    pub fn value(self) -> std::string::String {
        self.0
            .children()
            .filter(|node| node.token_kind() == TokenKind::StrVal)
            .map(|node| {
                node.text()
                    .as_str()
                    .trim_start_matches('"')
                    .trim_end_matches('"')
            })
            .collect::<Vec<_>>()
            .join("")
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
        let text = self.0.children().next()?.text();
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
    pub fn value_list(self) -> Option<ValueList<'a>> {
        self.0.cast_first_match()
    }
}

node!(ValueList);

impl<'a> ValueList<'a> {
    pub fn values(self) -> impl DoubleEndedIterator<Item = Value<'a>> {
        self.0.cast_all_matches()
    }
}

node!(List);

impl<'a> List<'a> {
    pub fn value_list(self) -> Option<ValueList<'a>> {
        self.0.cast_first_match()
    }
}

node!(Dag);

impl<'a> Dag<'a> {
    pub fn arg_list(self) -> Option<DagArgList<'a>> {
        self.0.cast_first_match()
    }
}

node!(DagArgList);

impl<'a> DagArgList<'a> {
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
