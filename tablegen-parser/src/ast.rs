use crate::{
    language::{Language, SyntaxNode},
    syntax_kind::SyntaxKind,
};
use ecow::EcoString;
use rowan::SyntaxNodeChildren;
use std::iter::FilterMap;

pub use rowan::ast::AstNode;

macro_rules! node {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name(SyntaxNode);

        impl AstNode for $name {
            type Language = Language;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$name
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    SyntaxKind::$name => Some(Self(node)),
                    _ => None,
                }
            }

            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                &self.0
            }
        }
    };
}

macro_rules! node_enum {
  ($name:ident, [$($item:ident),*]) => {
    #[derive(Debug)]
    pub enum $name {
        $($item($item),)*
    }

    impl AstNode for $name {
        type Language = Language;

        fn can_cast(kind: SyntaxKind) -> bool {
            matches!(kind, $(SyntaxKind::$item)|*)
        }

        fn cast(node: SyntaxNode) -> Option<Self> {
            match node.kind() {
                $(SyntaxKind::$item => $item::cast(node).map(Self::$item),)*
                _ => None,
            }
        }

        fn syntax(&self) -> &SyntaxNode {
            match self {
                $(Self::$item(v) => v.syntax(),)*
            }
        }
    }
  };
}

// FIXME
trait Castable {
    fn cast_first_match<T>(&self) -> Option<T>
    where
        T: AstNode<Language = Language>;

    fn cast_all_matches<T>(
        &self,
    ) -> FilterMap<SyntaxNodeChildren<Language>, fn(SyntaxNode) -> Option<T>>
    where
        T: AstNode<Language = Language>;
}

impl Castable for SyntaxNode {
    fn cast_first_match<T: AstNode<Language = Language>>(&self) -> Option<T> {
        self.children().find_map(T::cast)
    }

    fn cast_all_matches<T>(
        &self,
    ) -> FilterMap<SyntaxNodeChildren<Language>, fn(SyntaxNode) -> Option<T>>
    where
        T: AstNode<Language = Language>,
    {
        self.children().filter_map(T::cast)
    }
}

node!(Root);

impl Root {
    pub fn statement_list(&self) -> Option<StatementList> {
        self.0.cast_first_match()
    }
}

node!(StatementList);

impl StatementList {
    pub fn statements(&self) -> impl Iterator<Item = Statement> {
        self.0.cast_all_matches()
    }
}

node_enum!(
    Statement,
    [Include, Assert, Class, Def, Defm, Defset, Defvar, Foreach, If, Let, MultiClass]
);

node!(Include);

impl Include {
    pub fn path(&self) -> Option<String> {
        self.0.cast_first_match()
    }
}

node!(Class);

impl Class {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn template_arg_list(&self) -> Option<TemplateArgList> {
        self.0.cast_first_match()
    }

    pub fn record_body(&self) -> Option<RecordBody> {
        self.0.cast_first_match()
    }
}

node!(Def);

impl Def {
    pub fn name(&self) -> Option<Value> {
        self.0.cast_first_match()
    }

    pub fn record_body(&self) -> Option<RecordBody> {
        self.0.cast_first_match()
    }
}

node!(Let);

impl Let {
    pub fn let_list(&self) -> Option<LetList> {
        self.0.cast_first_match()
    }

    pub fn body(&self) -> impl Iterator<Item = Statement> {
        self.0.cast_all_matches()
    }
}

node!(LetList);

impl LetList {
    pub fn items(&self) -> impl Iterator<Item = LetItem> {
        self.0.cast_all_matches()
    }
}

node!(LetItem);

impl LetItem {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn range_list(&self) -> Option<RangeList> {
        self.0.cast_first_match()
    }

    pub fn value(&self) -> Option<Value> {
        self.0.cast_first_match()
    }
}

node!(MultiClass);

impl MultiClass {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn template_arg_list(&self) -> Option<TemplateArgList> {
        self.0.cast_first_match()
    }

    pub fn parent_class_list(&self) -> Option<ParentClassList> {
        self.0.cast_first_match()
    }

    pub fn statement_list(&self) -> Option<StatementList> {
        self.0.cast_first_match()
    }
}

node!(Defm);

impl Defm {
    pub fn name(&self) -> Option<Value> {
        self.0.cast_first_match()
    }

    pub fn parent_class_list(&self) -> Option<ParentClassList> {
        self.0.cast_first_match()
    }
}

node!(Defset);

impl Defset {
    pub fn r#type(&self) -> Option<Type> {
        self.0.cast_first_match()
    }

    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn statement_list(&self) -> Option<StatementList> {
        self.0.cast_first_match()
    }
}

node!(Defvar);

impl Defvar {
    pub fn name(&self) -> Option<Value> {
        self.0.cast_first_match()
    }

    pub fn value(&self) -> Option<Value> {
        self.0.cast_first_match()
    }
}

node!(Foreach);

impl Foreach {
    pub fn iterator(&self) -> Option<ForeachIterator> {
        self.0.cast_first_match()
    }

    pub fn body(&self) -> Option<StatementList> {
        self.0.cast_first_match()
    }
}

node!(ForeachIterator);

impl ForeachIterator {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn init(&self) -> Option<ForeachIteratorInit> {
        self.0.cast_first_match()
    }
}

node_enum!(ForeachIteratorInit, [RangeList, RangePiece, Value]);

node!(If);

impl If {
    pub fn condition(&self) -> Option<Value> {
        self.0.cast_first_match()
    }

    pub fn statement_list(&self) -> Option<StatementList> {
        self.0.cast_first_match()
    }
}

node!(Assert);

impl Assert {
    pub fn condition(&self) -> Option<Value> {
        self.0.cast_first_match()
    }

    pub fn message(&self) -> Option<Value> {
        self.0.cast_first_match()
    }
}

node!(TemplateArgList);

impl TemplateArgList {
    pub fn args(&self) -> impl Iterator<Item = TemplateArgDecl> {
        self.0.cast_all_matches()
    }
}

node!(TemplateArgDecl);

impl TemplateArgDecl {
    pub fn r#type(&self) -> Option<Type> {
        self.0.cast_first_match()
    }

    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn value(&self) -> Option<Value> {
        self.0.cast_first_match()
    }
}

node!(RecordBody);

impl RecordBody {
    pub fn parent_class_list(&self) -> Option<ParentClassList> {
        self.0.cast_first_match()
    }

    pub fn body(&self) -> Option<Body> {
        self.0.cast_first_match()
    }
}

node!(ParentClassList);

impl ParentClassList {
    pub fn classes(&self) -> impl Iterator<Item = ClassRef> {
        self.0.cast_all_matches()
    }
}

node!(ClassRef);

impl ClassRef {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn arg_value_list(&self) -> Option<ArgValueList> {
        self.0.cast_first_match()
    }
}

node!(ArgValueList);

impl ArgValueList {
    pub fn positional(&self) -> Option<PositionalArgValueList> {
        self.0.cast_first_match()
    }

    pub fn named(&self) -> Option<NamedArgValueList> {
        self.0.cast_first_match()
    }
}

node!(PositionalArgValueList);

impl PositionalArgValueList {
    pub fn values(&self) -> impl Iterator<Item = Value> {
        self.0.cast_all_matches()
    }
}

node!(NamedArgValueList);

impl NamedArgValueList {
    pub fn values(&self) -> impl Iterator<Item = NamedArgValue> {
        self.0.cast_all_matches()
    }
}

node!(NamedArgValue);

impl NamedArgValue {
    pub fn name(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn value(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(1)
    }
}

node!(Body);

impl Body {
    pub fn items(&self) -> impl Iterator<Item = BodyItem> {
        self.0.cast_all_matches()
    }
}

node_enum!(BodyItem, [FieldDef, FieldLet, Defvar, Assert]);

node!(FieldDef);

impl FieldDef {
    pub fn r#type(&self) -> Option<Type> {
        self.0.cast_first_match()
    }

    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn value(&self) -> Option<Value> {
        self.0.cast_first_match()
    }
}

node!(CodeType);

node!(FieldLet);

impl FieldLet {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }

    pub fn value(&self) -> Option<Value> {
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

impl BitsType {
    pub fn length(&self) -> Option<Integer> {
        self.0.cast_first_match()
    }
}

node!(ListType);

impl ListType {
    pub fn inner_type(&self) -> Option<Type> {
        self.0.cast_first_match()
    }
}

node!(ClassId);

impl ClassId {
    pub fn name(&self) -> Option<Identifier> {
        self.0.cast_first_match()
    }
}

node!(Value);

impl Value {
    pub fn inner_values(&self) -> impl Iterator<Item = InnerValue> {
        self.0.cast_all_matches()
    }
}

node!(InnerValue);

impl InnerValue {
    pub fn simple_value(&self) -> Option<SimpleValue> {
        self.0.cast_first_match()
    }

    pub fn suffixes(&self) -> impl Iterator<Item = ValueSuffix> {
        self.0.cast_all_matches()
    }
}

node_enum!(ValueSuffix, [RangeSuffix, SliceSuffix, FieldSuffix]);

node!(RangeSuffix);

impl RangeSuffix {
    pub fn range_list(&self) -> Option<RangeList> {
        self.0.cast_first_match()
    }
}

node!(RangeList);

impl RangeList {
    pub fn pieces(&self) -> impl Iterator<Item = RangePiece> {
        self.0.cast_all_matches()
    }
}

node!(RangePiece);

impl RangePiece {
    pub fn start(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn end(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(1)
    }
}

node!(SliceSuffix);

impl SliceSuffix {
    pub fn element_list(&self) -> Option<SliceElements> {
        self.0.cast_first_match()
    }
}

node!(SliceElements);

impl SliceElements {
    pub fn elements(&self) -> impl Iterator<Item = SliceElement> {
        self.0.cast_all_matches()
    }
}

node!(SliceElement);

impl SliceElement {
    pub fn start(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn end(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(1)
    }
}

node!(FieldSuffix);

impl FieldSuffix {
    pub fn name(&self) -> Option<Identifier> {
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

impl Integer {
    pub fn value(&self) -> Option<i64> {
        let token = self.0.first_token()?;
        let text = token.text();
        if let Some(rest) = text.strip_prefix("0x") {
            i64::from_str_radix(rest, 16).ok()
        } else {
            i64::from_str_radix(text, 10).ok()
        }
    }
}

node!(String);

impl String {
    pub fn value(&self) -> std::string::String {
        self.0
            .children_with_tokens()
            .filter(|node| node.kind() == SyntaxKind::StrVal)
            .filter_map(|node| {
                node.as_token().map(|token| {
                    token
                        .text()
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string()
                })
            })
            .collect::<Vec<_>>()
            .join("")
    }
}

node!(Code);

impl Code {
    pub fn value(&self) -> Option<EcoString> {
        self.0.first_token().map(|token| {
            token
                .text()
                .trim_start_matches("[{")
                .trim_end_matches("}]")
                .trim()
                .into()
        })
    }
}

node!(Boolean);

impl Boolean {
    pub fn value(&self) -> Option<bool> {
        let token = self.0.first_token()?;
        match token.text() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        }
    }
}

node!(Uninitialized);

node!(Bits);

impl Bits {
    pub fn value_list(&self) -> Option<ValueList> {
        self.0.cast_first_match()
    }
}

node!(ValueList);

impl ValueList {
    pub fn values(&self) -> impl Iterator<Item = Value> {
        self.0.cast_all_matches()
    }
}

node!(List);

impl List {
    pub fn value_list(&self) -> Option<ValueList> {
        self.0.cast_first_match()
    }
}

node!(Dag);

impl Dag {
    pub fn arg_list(&self) -> Option<DagArgList> {
        self.0.cast_first_match()
    }
}

node!(DagArgList);

impl DagArgList {
    pub fn args(&self) -> impl Iterator<Item = DagArg> {
        self.0.cast_all_matches()
    }
}

node!(DagArg);

impl DagArg {
    pub fn value(&self) -> Option<Value> {
        self.0.cast_first_match()
    }

    pub fn var_name(&self) -> Option<VarName> {
        self.0.cast_first_match()
    }
}

node!(VarName);

impl VarName {
    pub fn value(&self) -> Option<EcoString> {
        Some(self.0.first_token()?.text().into())
    }
}

node!(Identifier);

impl Identifier {
    pub fn value(&self) -> Option<EcoString> {
        Some(self.0.first_token()?.text().into())
    }
}

node!(BangOperator);

impl BangOperator {
    pub fn kind(&self) -> Option<SyntaxKind> {
        Some(self.0.first_token()?.kind())
    }

    pub fn values(&self) -> impl Iterator<Item = Value> {
        self.0.cast_all_matches()
    }
}

node!(CondOperator);

impl CondOperator {
    pub fn clauses(&self) -> impl Iterator<Item = CondClause> {
        self.0.cast_all_matches()
    }
}

node!(CondClause);

impl CondClause {
    pub fn condition(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(0)
    }

    pub fn value(&self) -> Option<Value> {
        self.0.cast_all_matches().nth(1)
    }
}
