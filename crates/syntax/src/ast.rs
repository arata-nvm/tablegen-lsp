use ecow::EcoString;
pub use rowan::ast::AstNode;

use crate::parser::TextRange;
use crate::syntax_kind::SyntaxKind;
use crate::{Language, SyntaxNode};

macro_rules! asts {
    () => {};
    ($name:ident $body:tt; $($rest:tt)*) => {
        ast!($name $body);
        asts!($($rest)*);
    };
}

macro_rules! ast {
    ($name:ident { $($field:tt)* }) => {
        #[derive(Debug, Clone)]
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

        impl $name {
            ast_field!($($field)*);
        }
    };
    ($name:ident [ $($item:ident,)* ]) => {
        #[derive(Debug, Clone)]
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

macro_rules! ast_field {
    () => {};
    ($field:ident: $ast:ident, $($rest:tt)*) => {
        pub fn $field(&self) -> Option<$ast> {
            rowan::ast::support::child(&self.0)
        }
        ast_field!($($rest)*);
    };
    ($field:ident: [$ast:ident], $($rest:tt)*) => {
        pub fn $field(&self) -> impl Iterator<Item = $ast> {
            rowan::ast::support::children(&self.0)
        }
        ast_field!($($rest)*);
    };
    ($field:ident[$index:tt]: $ast:ident, $($rest:tt)*) => {
        pub fn $field(&self) -> Option<$ast> {
            rowan::ast::support::children(&self.0).nth($index)
        }
        ast_field!($($rest)*);
    };
    ($($item:item)*) => {
        $($item)*
    }
}

asts! {
    Root {
        statement_list: StatementList,
    };
    StatementList {
        statements: [Statement],
    };
    Statement [
        Include,
        Assert,
        Class,
        Def,
        Defm,
        Defset,
        Defvar,
        Foreach,
        If,
        Let,
        MultiClass,
    ];
    Include {
        path: String,
    };
    Class {
        name: Identifier,
        template_arg_list: TemplateArgList,
        record_body: RecordBody,
    };
    Def {
        name: Value,
        record_body: RecordBody,
    };
    Let {
        let_list: LetList,
        statement_list: StatementList,
    };
    LetList {
        items: [LetItem],
    };
    LetItem {
        name: Identifier,
        range_list: RangeList,
        value: Value,
    };
    MultiClass {
        name: Identifier,
        template_arg_list: TemplateArgList,
        parent_class_list: ParentClassList,
        statement_list: StatementList,
    };
    Defm {
        name: Value,
        parent_class_list: ParentClassList,
    };
    Defset {
        r#type: Type,
        name: Identifier,
        statement_list: StatementList,
    };
    Defvar {
        name: Identifier,
        value: Value,
    };
    Foreach {
        iterator: ForeachIterator,
        body: StatementList,
    };
    ForeachIterator {
        name: Identifier,
        init: ForeachIteratorInit,
    };
    ForeachIteratorInit [
        RangeSuffix,
        RangePiece,
        Value,
    ];
    If {
        condition: Value,
        statement_list: StatementList,
    };
    Assert {
        condition: Value,
        message: Value,
    };
    TemplateArgList {
        args: [TemplateArgDecl],
    };
    TemplateArgDecl {
        r#type: Type,
        name: Identifier,
        value: Value,
    };
    RecordBody {
        parent_class_list: ParentClassList,
        body: Body,
    };
    ParentClassList {
        classes: [ClassRef],
    };
    ClassRef {
        name: Identifier,
        arg_value_list: ArgValueList,
    };
    ArgValueList {
        positional: PositionalArgValueList,
        named: NamedArgValueList,
    };
    PositionalArgValueList {
        values: [Value],
    };
    NamedArgValueList {
        values: [NamedArgValue],
    };
    NamedArgValue {
        name[0]: Value,
        value[1]: Value,
    };
    Body {
        items: [BodyItem],
    };
    BodyItem [
        FieldDef,
        FieldLet,
        Defvar,
        Assert,
    ];
    FieldDef {
        r#type: Type,
        name: Identifier,
        value: Value,
    };
    FieldLet {
        name: Identifier,
        value: Value,
    };
    Type [
        BitType,
        IntType,
        StringType,
        DagType,
        BitsType,
        ListType,
        ClassId,
        CodeType,
    ];
    BitType {};
    IntType {};
    StringType {};
    DagType {};
    BitsType {
        length: Integer,
    };
    ListType {
        inner_type: Type,
    };
    CodeType {};
    ClassId {
        name: Identifier,
    };
    Value {
        inner_values: [InnerValue],
    };
    InnerValue {
        simple_value: SimpleValue,
        suffixes: [ValueSuffix],
    };
    ValueSuffix [
        RangeSuffix,
        SliceSuffix,
        FieldSuffix,
    ];
    RangeSuffix {
        range_list: RangeList,
    };
    RangeList {
        pieces: [RangePiece],
    };
    RangePiece {
        start[0]: Integer,
        end[1]: Integer,
    };
    SliceSuffix {
        element_list: SliceElements,
        pub fn is_single_element(&self) -> bool {
            let Some(list) = self.element_list() else {
                return false;
            };
            let elements: Vec<SliceElement> = list.elements().collect();
            let num_colon = self
                .0
                .children_with_tokens()
                .filter(|element| element.kind() == SyntaxKind::Colon)
                .count();
            elements.len() == 1 && elements[0].end().is_none() && num_colon == 0
        }
    };
    SliceElements {
       elements: [SliceElement],
    };
    SliceElement {
        start[0]: Value,
        end[1]: Value,
    };
    FieldSuffix {
        name: Identifier,
    };
    SimpleValue [
        Integer,
        String,
        Code,
        Boolean,
        Uninitialized,
        Bits,
        List,
        Dag,
        Identifier,
        ClassValue,
        BangOperator,
        CondOperator,
    ];
    Integer {
        pub fn value(&self) -> Option<i64> {
            let token = self.0.first_token()?;
            let text = token.text();
            if let Some(rest) = text.strip_prefix("0x") {
                i64::from_str_radix(rest, 16).ok()
            } else {
                text.parse::<i64>().ok()
            }
        }
    };
    String {
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
    };
    Code {
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
    };
    Boolean {
        pub fn value(&self) -> Option<bool> {
            let token = self.0.first_token()?;
            match token.text() {
                "true" => Some(true),
                "false" => Some(false),
                _ => None,
            }
        }
    };
    Uninitialized {};
    Bits {
        value_list: ValueList,
    };
    List {
        value_list: ValueList,
    };
    ValueList {
        values: [Value],
    };
    Dag {
        operator: DagArg,
        arg_list: DagArgList,
    };
    DagArgList {
        args: [DagArg],
    };
    DagArg {
        value: Value,
        var_name: VarName,
    };
    VarName {
        pub fn value(&self) -> Option<EcoString> {
            Some(self.0.first_token()?.text().into())
        }
    };
    Identifier {
        pub fn value(&self) -> Option<EcoString> {
            Some(self.0.first_token()?.text().into())
        }

        pub fn range(&self) -> Option<TextRange> {
            Some(self.0.first_token()?.text_range())
        }
    };
    ClassValue {
        name: Identifier,
        arg_value_list: ArgValueList,
    };
    BangOperator {
        r#type: Type,
        values: [Value],
        pub fn kind(&self) -> Option<SyntaxKind> {
            Some(self.0.first_token()?.kind())
        }
    };
    CondOperator {
        clauses: [CondClause],
    };
    CondClause {
        condition[0]: Value,
        value[1]: Value,
    };
}

#[cfg(test)]
mod tests {
    use rowan::ast::AstNode;

    use crate::Language;

    use super::*;

    fn parse<T: AstNode<Language = Language>>(text: &str) -> T {
        let root_node = crate::parse(text).root_node();
        root_node.descendants().find_map(T::cast).unwrap()
    }

    #[test]
    fn root() {
        let node = parse::<Root>("class Foo;");
        assert!(node.statement_list().is_some());
    }

    #[test]
    fn statement_list() {
        let node = parse::<StatementList>("class Foo; class Bar;");
        assert_eq!(node.statements().count(), 2);
    }

    #[test]
    fn include() {
        let node = parse::<Include>("include \"foo.td\"");
        assert!(node.path().is_some());
    }

    #[test]
    fn class() {
        let node = parse::<Class>("class Foo<int A = 1> : Bar<A> { int A = 1; let B = 2; defvar C = 3; assert true, \"\"; };");
        assert!(node.name().is_some());
        assert!(node.template_arg_list().is_some());
        assert!(node.record_body().is_some());

        // template_arg_list
        let args: Vec<TemplateArgDecl> = node.template_arg_list().unwrap().args().collect();
        assert_eq!(args.len(), 1);

        // template_arg_decl
        let arg = args.first().unwrap();
        assert!(arg.r#type().is_some());
        assert!(arg.name().is_some());
        assert!(arg.value().is_some());

        // record_body
        let record_body = node.record_body().unwrap();
        assert!(record_body.parent_class_list().is_some());
        assert!(record_body.body().is_some());

        // parent_class_list
        let classes: Vec<ClassRef> = record_body.parent_class_list().unwrap().classes().collect();
        assert_eq!(classes.len(), 1);

        // class_ref
        let class_ref = classes.first().unwrap();
        assert!(class_ref.name().is_some());
        assert!(class_ref.arg_value_list().is_some());

        // arg_value_list
        let list = class_ref.arg_value_list().unwrap();
        assert!(list.positional().is_some());

        // positional_arg_value_list
        let positional: Vec<Value> = list.positional().unwrap().values().collect();
        assert_eq!(positional.len(), 1);

        // body
        let items: Vec<BodyItem> = record_body.body().unwrap().items().collect();

        // field_def
        let BodyItem::FieldDef(field_def) = items.first().unwrap() else {
            panic!();
        };
        assert!(field_def.r#type().is_some());
        assert!(field_def.name().is_some());
        assert!(field_def.value().is_some());

        // field_let
        let BodyItem::FieldLet(field_let) = items.get(1).unwrap() else {
            panic!();
        };
        assert!(field_let.name().is_some());
        assert!(field_let.value().is_some());

        // defvar
        let BodyItem::Defvar(_) = items.get(2).unwrap() else {
            panic!();
        };

        // assert
        let BodyItem::Assert(_) = items.get(3).unwrap() else {
            panic!();
        };
    }

    #[test]
    fn def() {
        let node = parse::<Def>("def foo: Foo");
        assert!(node.name().is_some());
        assert!(node.record_body().is_some());
    }

    #[test]
    fn r#let() {
        let node = parse::<Let>("let foo<1> = 1 {}");
        assert!(node.let_list().is_some());
        assert!(node.statement_list().is_some());

        // let_list
        let items: Vec<LetItem> = node.let_list().unwrap().items().collect();
        assert_eq!(items.len(), 1);

        // let_item
        let item = items.first().unwrap();
        assert!(item.name().is_some());
        assert!(item.range_list().is_some());
        assert!(item.value().is_some());
    }

    #[test]
    fn multi_class() {
        let node = parse::<MultiClass>("multiclass Foo<int A>: Bar {}");
        assert!(node.name().is_some());
        assert!(node.template_arg_list().is_some());
        assert!(node.parent_class_list().is_some());
        assert!(node.statement_list().is_some());
    }

    #[test]
    fn defm() {
        let node = parse::<Defm>("defm foo: Foo;");
        assert!(node.name().is_some());
        assert!(node.parent_class_list().is_some());
    }

    #[test]
    fn defset() {
        let node = parse::<Defset>("defset list<Foo> Foos = [];");
        assert!(node.r#type().is_some());
        assert!(node.name().is_some());
        assert!(node.statement_list().is_some());
    }

    #[test]
    fn defvar() {
        let node = parse::<Defvar>("defvar foo = 1;");
        assert!(node.name().is_some());
        assert!(node.value().is_some());
    }

    #[test]
    fn foreach() {
        let node = parse::<Foreach>("foreach i = {1-4} in {}");
        assert!(node.iterator().is_some());
        assert!(node.body().is_some());

        // foreach_iterator
        let iter = node.iterator().unwrap();
        assert!(iter.name().is_some());
        assert!(iter.init().is_some());
    }

    #[test]
    fn r#if() {
        let node = parse::<If>("if 1 then {} else {}");
        assert!(node.condition().is_some());
        assert!(node.statement_list().is_some());
    }

    #[test]
    fn assert() {
        let node = parse::<Assert>("assert 1, \"\";");
        assert!(node.condition().is_some());
        assert!(node.message().is_some());
    }

    #[test]
    fn r#type() {
        let node = parse::<Class>(
            "class Foo<bit A, int B, string C, dag D, bits<4> E, list<int> F, Bar G, code H>;",
        );
        let types: Vec<Type> = node
            .template_arg_list()
            .unwrap()
            .args()
            .map(|arg| arg.r#type().unwrap())
            .collect();

        // bit_type
        let Type::BitType(_) = types.first().unwrap() else {
            panic!();
        };

        // int_type
        let Type::IntType(_) = types.get(1).unwrap() else {
            panic!();
        };

        // string_type
        let Type::StringType(_) = types.get(2).unwrap() else {
            panic!();
        };
        // dag_type
        let Type::DagType(_) = types.get(3).unwrap() else {
            panic!();
        };
        // bits_type
        let Type::BitsType(bits) = types.get(4).unwrap() else {
            panic!();
        };
        assert!(bits.length().is_some());

        // list_type
        let Type::ListType(list) = types.get(5).unwrap() else {
            panic!();
        };
        assert!(list.inner_type().is_some());

        // class_id
        let Type::ClassId(class_id) = types.get(6).unwrap() else {
            panic!();
        };
        assert!(class_id.name().is_some());

        // code_type
        let Type::CodeType(_) = types.get(7).unwrap() else {
            panic!();
        };
    }

    #[test]
    fn value() {
        let node = parse::<Defvar>("defvar a = \"foo\" # \"bar\"");
        let values: Vec<InnerValue> = node.value().unwrap().inner_values().collect();
        assert_eq!(values.len(), 2);
    }

    #[test]
    fn inner_value() {
        let node = parse::<Defvar>("defvar a = a{0-3}[0-3].field;");
        let inner_value = node.value().unwrap().inner_values().next().unwrap();
        assert!(inner_value.simple_value().is_some());

        // range_suffix
        let ValueSuffix::RangeSuffix(range_suffix) = inner_value.suffixes().next().unwrap() else {
            panic!();
        };
        assert!(range_suffix.range_list().is_some());

        // range_list
        let pieces: Vec<RangePiece> = range_suffix.range_list().unwrap().pieces().collect();
        assert_eq!(pieces.len(), 1);

        // range_piece
        let piece = pieces.first().unwrap();
        assert!(piece.start().is_some());
        assert!(piece.end().is_some());

        // slice_suffix
        let ValueSuffix::SliceSuffix(slice_suffix) = inner_value.suffixes().nth(1).unwrap() else {
            panic!();
        };
        assert!(slice_suffix.element_list().is_some());

        // slice_elements
        let elements: Vec<SliceElement> = slice_suffix.element_list().unwrap().elements().collect();
        assert_eq!(elements.len(), 1);

        let element = elements.first().unwrap();
        assert!(element.start().is_some());
        assert!(element.end().is_some());

        // field_suffix
        let ValueSuffix::FieldSuffix(field_suffix) = inner_value.suffixes().nth(2).unwrap() else {
            panic!();
        };
        assert!(field_suffix.name().is_some());
    }

    #[test]
    fn simple_value() {
        fn parse_simple_value(text: &str) -> SimpleValue {
            let node = parse::<Defvar>(&format!("defvar a = {};", text));
            node.value()
                .unwrap()
                .inner_values()
                .next()
                .unwrap()
                .simple_value()
                .unwrap()
        }

        // integer
        let SimpleValue::Integer(integer) = parse_simple_value("1") else {
            panic!();
        };
        assert!(integer.value().is_some());

        // string
        let SimpleValue::String(string) = parse_simple_value("\"foo\"") else {
            panic!();
        };
        assert!(!string.value().is_empty());

        // code
        let SimpleValue::Code(code) = parse_simple_value("[{foo}]") else {
            panic!();
        };
        assert!(code.value().is_some());

        // boolean
        let SimpleValue::Boolean(boolean) = parse_simple_value("true") else {
            panic!();
        };
        assert!(boolean.value().is_some());

        // uninitialized
        let SimpleValue::Uninitialized(_) = parse_simple_value("?") else {
            panic!();
        };

        // bits
        let SimpleValue::Bits(bits) = parse_simple_value("{0, 1}") else {
            panic!();
        };
        assert!(bits.value_list().is_some());

        // list
        let SimpleValue::List(list) = parse_simple_value("[1, 2, 3, 4]") else {
            panic!();
        };
        assert!(list.value_list().is_some());

        // dag
        let SimpleValue::Dag(dag) = parse_simple_value("(add:$op 1, 2)") else {
            panic!();
        };
        assert!(dag.operator().is_some());
        assert!(dag.arg_list().is_some());

        // dag_arg
        let dag_arg = dag.operator().unwrap();
        assert!(dag_arg.value().is_some());
        assert!(dag_arg.var_name().is_some());

        // dag_arg_list
        let args: Vec<DagArg> = dag.arg_list().unwrap().args().collect();
        assert_eq!(args.len(), 2);

        // identifier
        let SimpleValue::Identifier(identifier) = parse_simple_value("foo") else {
            panic!();
        };
        assert!(identifier.value().is_some());

        // class_value
        let SimpleValue::ClassValue(class_value) = parse_simple_value("Foo<1>") else {
            panic!();
        };
        assert!(class_value.name().is_some());
        assert!(class_value.arg_value_list().is_some());

        // bang_operator
        let SimpleValue::BangOperator(bang_operator) = parse_simple_value("!add(1, 2)") else {
            panic!();
        };
        assert!(bang_operator.kind().is_some());

        let values: Vec<Value> = bang_operator.values().collect();
        assert_eq!(values.len(), 2);

        // cond_operator
        let SimpleValue::CondOperator(cond_operator) =
            parse_simple_value("!cond(false : 0, true : 1)")
        else {
            panic!();
        };
        let clauses: Vec<CondClause> = cond_operator.clauses().collect();
        assert_eq!(clauses.len(), 2);

        // cond_clause
        let clause = clauses.first().unwrap();
        assert!(clause.condition().is_some());
        assert!(clause.value().is_some());
    }
}
