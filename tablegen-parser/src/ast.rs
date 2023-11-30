use ecow::EcoString;
pub use rowan::ast::AstNode;

use crate::language::{Language, SyntaxNode};
use crate::parser::TextRange;
use crate::syntax_kind::SyntaxKind;

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
        start[0]: Value,
        end[1]: Value,
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
                i64::from_str_radix(text, 10).ok()
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
