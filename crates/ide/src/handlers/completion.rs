use ecow::EcoString;
use syntax::{
    SyntaxNode, SyntaxNodePtr,
    ast::{self, AstNode},
};

use crate::{
    bang_operator,
    db::IndexDatabase,
    file_system::{FilePosition, SourceUnitId},
    index::Index,
    symbol_map::{
        SymbolMap,
        record::{AsRecordData, RecordFieldId},
        template_arg::TemplateArgumentId,
        typ::Type,
    },
    utils::{self, SyntaxNodeExt},
};
use crate::{
    symbol_map::{class::ClassId, def::DefId, record::RecordId},
    utils::DefNameType,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Eq, PartialEq)]
pub struct CompletionItem {
    pub label: String,
    pub insert_text_snippet: Option<String>,
    pub detail: String,
    pub documentation: Option<String>,
    pub kind: CompletionItemKind,
    pub sort_text: Option<String>,
    pub(crate) item_type: Option<Type>,
}

impl CompletionItem {
    fn new_simple(
        label: impl Into<String>,
        detail: impl Into<String>,
        kind: CompletionItemKind,
    ) -> Self {
        Self {
            label: label.into(),
            insert_text_snippet: None,
            detail: detail.into(),
            documentation: None,
            kind,
            sort_text: None,
            item_type: None,
        }
    }

    fn with_type(mut self, typ: Type) -> Self {
        self.item_type = Some(typ);
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompletionItemKind {
    Keyword,
    Type,
    Class,
    TemplateArgument,
    Field,
    Def,
    Defset,
    Multiclass,
    Variable,
}

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    pos: FilePosition,
    trigger_char: Option<String>,
) -> Option<Vec<CompletionItem>> {
    let index = db.index(source_unit_id);
    exec_with_index(db, pos, trigger_char, &index)
}

pub fn exec_with_index(
    db: &dyn IndexDatabase,
    pos: FilePosition,
    trigger_char: Option<String>,
    index: &Index,
) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(pos.file);
    let Index {
        symbol_map,
        resolved_types,
        ..
    } = index;

    let root_node = parse.syntax_node();
    let token_at_pos = root_node.token_at_offset(pos.position).left_biased()?;
    let node_at_pos = token_at_pos.parent()?;

    let mut ctx = CompletionContext::new(db);

    if trigger_char == Some("!".into()) {
        ctx.complete_bang_operators();
        return Some(ctx.finish(symbol_map));
    }

    if node_at_pos
        .ancestor_within::<ast::StatementList>(2)
        .is_some()
    {
        ctx.complete_toplevel_keywords();
        return Some(ctx.finish(symbol_map));
    }

    if node_at_pos.ancestor_within::<ast::Type>(2).is_some() {
        ctx.complete_primitive_types();
        ctx.complete_classes(symbol_map, &HashSet::new(), false);
        return Some(ctx.finish(symbol_map));
    }

    if node_at_pos.ancestor_within::<ast::ClassRef>(2).is_some() {
        let exclude = find_class_names_to_exclude(&node_at_pos).unwrap_or_default();
        if node_at_pos.ancestor::<ast::Defm>().is_some() {
            ctx.complete_multiclasses(symbol_map, &exclude);
        } else {
            ctx.complete_classes(symbol_map, &exclude, false);
        }
        return Some(ctx.finish(symbol_map));
    }

    if let Some(field_let) = node_at_pos.ancestor::<ast::FieldLet>()
        && !field_let.has_equal()
    {
        if let Some(class_id) = find_class(&node_at_pos, symbol_map) {
            ctx.complete_record_fields(symbol_map, &[class_id.into()], None);
        } else if let Some(def_id) = find_def(&node_at_pos, symbol_map) {
            ctx.complete_record_fields(symbol_map, &[def_id.into()], None);
        } else if let Some(parents) = find_def_parent_classes(&node_at_pos, symbol_map) {
            ctx.complete_record_fields(symbol_map, &parents, None);
        }

        return Some(ctx.finish(symbol_map));
    }

    if node_at_pos.ancestor::<ast::FieldSuffix>().is_some() {
        let typ = find_typ_of_field_suffix(&node_at_pos, resolved_types)?;
        let record_ids = typ.record_to_ids()?;
        if let Some(expected_type) = find_expected_type_for_inner_value(&node_at_pos, symbol_map) {
            ctx.set_expected_type(expected_type);
        }
        ctx.complete_record_fields(symbol_map, &record_ids, None);
        return Some(ctx.finish(symbol_map));
    }

    if node_at_pos.ancestor_within::<ast::InnerValue>(2).is_some() {
        if let Some(expected_type) = find_expected_type_for_inner_value(&node_at_pos, symbol_map) {
            ctx.set_expected_type(expected_type);
        }
        ctx.complete_primitive_values();
        ctx.complete_classes(symbol_map, &HashSet::new(), true);
        ctx.complete_defs(symbol_map);
        ctx.complete_defsets(symbol_map);

        let variables = collect_variables_from_ancestors(&node_at_pos);
        ctx.complete_variables(symbol_map, variables);

        if let Some(class_id) = find_class(&node_at_pos, symbol_map) {
            ctx.complete_template_arguments(symbol_map, class_id);

            let exclude = find_field_name_to_exclude(&node_at_pos);
            ctx.complete_record_fields(symbol_map, &[class_id.into()], exclude);
        }

        if let Some(def_id) = find_def(&node_at_pos, symbol_map) {
            let exclude = find_field_name_to_exclude(&node_at_pos);
            ctx.complete_record_fields(symbol_map, &[def_id.into()], exclude);
        } else if let Some(parents) = find_def_parent_classes(&node_at_pos, symbol_map) {
            let exclude = find_field_name_to_exclude(&node_at_pos);
            ctx.complete_record_fields(symbol_map, &parents, exclude);
        }

        return Some(ctx.finish(symbol_map));
    }

    Some(ctx.finish(symbol_map))
}

fn find_class(node: &SyntaxNode, symbol_map: &SymbolMap) -> Option<ClassId> {
    let class_stmt = node.ancestor::<ast::Class>()?;
    let class_name = class_stmt.name()?.value()?;
    let class_id = symbol_map.find_class(&class_name)?;
    Some(class_id)
}

fn find_def(node: &SyntaxNode, symbol_map: &SymbolMap) -> Option<DefId> {
    let def_stmt = node.ancestor::<ast::Def>()?;
    let def_name_type = utils::determine_def_type(&def_stmt)?;
    let DefNameType::Identifier(_, ident) = def_name_type else {
        return None;
    };
    let name = ident.value()?;
    symbol_map.find_def(&name)
}

fn find_template_arg(node: &SyntaxNode, symbol_map: &SymbolMap) -> Option<TemplateArgumentId> {
    let template_arg_decl = node.ancestor::<ast::TemplateArgDecl>()?;
    let template_arg_name = template_arg_decl.name()?.value()?;
    let class_id = find_class(node, symbol_map)?;
    let class = symbol_map.class(class_id);
    class.find_template_arg(&template_arg_name)
}

fn find_record_field(node: &SyntaxNode, symbol_map: &SymbolMap) -> Option<RecordFieldId> {
    let field_name = if let Some(field_def) = node.ancestor::<ast::FieldDef>() {
        field_def.name()?.value()?
    } else if let Some(field_let) = node.ancestor::<ast::FieldLet>() {
        field_let.name()?.value()?
    } else {
        return None;
    };

    if let Some(class_id) = find_class(node, symbol_map) {
        let class = symbol_map.class(class_id);
        class.find_field(symbol_map, &field_name)
    } else if let Some(def_id) = find_def(node, symbol_map) {
        let def = symbol_map.def(def_id);
        def.find_field(symbol_map, &field_name)
    } else {
        None
    }
}

fn find_def_parent_classes(node: &SyntaxNode, symbol_map: &SymbolMap) -> Option<Vec<RecordId>> {
    let def_stmt = node.ancestor::<ast::Def>()?;
    let record_body = def_stmt.record_body()?;
    let parent_class_list = record_body.parent_class_list()?;
    Some(
        parent_class_list
            .classes()
            .filter_map(|class_ref| class_ref.name())
            .filter_map(|name| name.value())
            .filter_map(|name| symbol_map.find_class(&name))
            .map(RecordId::Class)
            .collect::<Vec<_>>(),
    )
}

fn find_class_names_to_exclude(node: &SyntaxNode) -> Option<HashSet<EcoString>> {
    let parent_class_list = node.ancestor::<ast::ParentClassList>()?;

    let mut exclude = HashSet::new();

    // the class/multiclass itself
    // e.g. class Hoge : Fuga, Piyo, | -> {Hoge}
    if let Some(class) = node.ancestor::<ast::Class>()
        && let Some(name) = class.name().and_then(|n| n.value())
    {
        exclude.insert(name);
    }
    if let Some(multiclass) = node.ancestor::<ast::MultiClass>()
        && let Some(name) = multiclass.name().and_then(|n| n.value())
    {
        exclude.insert(name);
    }

    // already inherited classes/multiclasses
    // e.g. class Hoge : Fuga, Piyo, | -> {Fuga, Piyo}
    for class_ref in parent_class_list.classes() {
        if let Some(name) = class_ref.name().and_then(|n| n.value()) {
            exclude.insert(name);
        }
    }

    Some(exclude)
}

fn find_field_name_to_exclude(node: &SyntaxNode) -> Option<EcoString> {
    if let Some(field_def) = node.ancestor::<ast::FieldDef>()
        && let Some(name) = field_def.name().and_then(|n| n.value())
    {
        return Some(name);
    }

    if let Some(field_let) = node.ancestor::<ast::FieldLet>()
        && let Some(name) = field_let.name().and_then(|n| n.value())
    {
        return Some(name);
    }

    None
}

fn find_typ_of_field_suffix<'typ>(
    node: &SyntaxNode,
    resolved_types: &'typ HashMap<SyntaxNodePtr, Type>,
) -> Option<&'typ Type> {
    let inner_value = node.ancestor::<ast::InnerValue>()?;
    let suffixes = inner_value.suffixes().collect::<Vec<_>>();
    if suffixes.len() < 2 {
        // hoge.|
        let simple_value = inner_value.simple_value()?;
        let ptr = SyntaxNodePtr::new(simple_value.syntax());
        resolved_types.get(&ptr)
    } else {
        // hoge.fuga.| or hoge.fuga.piyo.|
        let suffix = suffixes.get(suffixes.len() - 2)?;
        let ptr = SyntaxNodePtr::new(suffix.syntax());
        resolved_types.get(&ptr)
    }
}

fn find_expected_type_for_inner_value(node: &SyntaxNode, symbol_map: &SymbolMap) -> Option<Type> {
    // FieldDef: int field1 = |
    // FieldLet: let field1 = |
    if let Some(field_id) = find_record_field(node, symbol_map) {
        return Some(symbol_map.record_field(field_id).typ.clone());
    }

    // TemplateArgDecl: class Foo<Bar x = |
    if let Some(template_arg_id) = find_template_arg(node, symbol_map) {
        return Some(symbol_map.template_arg(template_arg_id).typ.clone());
    }

    None
}

struct CompletionContext<'a> {
    items: Vec<CompletionItem>,
    db: &'a dyn IndexDatabase,
    expected_type: Option<Type>,
}

impl<'a> CompletionContext<'a> {
    fn new(db: &'a dyn IndexDatabase) -> Self {
        Self {
            items: Vec::new(),
            db,
            expected_type: None,
        }
    }

    fn set_expected_type(&mut self, expected_type: Type) {
        self.expected_type = Some(expected_type);
    }

    fn finish(mut self, symbol_map: &SymbolMap) -> Vec<CompletionItem> {
        let Some(expected_type) = self.expected_type else {
            return self.items;
        };

        for item in &mut self.items {
            let matches = item
                .item_type
                .as_ref()
                .map_or(false, |t| t.can_be_casted_to(symbol_map, &expected_type));
            let prefix = if matches { "0" } else { "1" };
            item.sort_text = Some(prefix.to_string());
        }
        self.items
    }

    fn complete_toplevel_keywords(&mut self) {
        const TOPLEVEL_KEYWORDS: [&str; 13] = [
            "assert",
            "class",
            "def",
            "dump",
            "foreach",
            "defm",
            "defset",
            "deftype",
            "defvar",
            "if",
            "include",
            "let",
            "multiclass",
        ];

        for &keyword in &TOPLEVEL_KEYWORDS {
            self.items.push(CompletionItem::new_simple(
                keyword,
                "",
                CompletionItemKind::Keyword,
            ));
        }
    }

    fn complete_primitive_types(&mut self) {
        const PRIMITIVE_TYPES: [&str; 5] = ["bit", "code", "dag", "int", "string"];
        for &ty in &PRIMITIVE_TYPES {
            self.items
                .push(CompletionItem::new_simple(ty, "", CompletionItemKind::Type));
        }

        let mut item = CompletionItem::new_simple("bits", "", CompletionItemKind::Type);
        item.insert_text_snippet = Some("bits<$1> $0".to_string());
        self.items.push(item);

        let mut item = CompletionItem::new_simple("list", "", CompletionItemKind::Type);
        item.insert_text_snippet = Some("list<$1> $0".to_string());
        self.items.push(item);
    }

    fn complete_primitive_values(&mut self) {
        const BOOLEAN_VALUES: [&str; 2] = ["false", "true"];
        for &value in &BOOLEAN_VALUES {
            self.items.push(
                CompletionItem::new_simple(value, "", CompletionItemKind::Keyword)
                    .with_type(Type::bit()),
            );
        }
    }

    fn complete_bang_operators(&mut self) {
        for meta in bang_operator::OPS {
            let snippet = if meta.needs_type_annotation {
                let args = (0..meta.min_args)
                    .map(|i| format!("${{{}}}", i + 2))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}<${{1}}>({args})$0", name = meta.name)
            } else {
                let args = (0..meta.min_args)
                    .map(|i| format!("${{{}}}", i + 2))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}({args})$0", name = meta.name)
            };

            let mut item = CompletionItem::new_simple(meta.name, "", CompletionItemKind::Keyword);
            item.insert_text_snippet = Some(snippet);
            item.documentation = Some(meta.documentation.into());
            self.items.push(item);
        }
    }

    fn complete_classes(
        &mut self,
        symbol_map: &SymbolMap,
        exclude: &HashSet<EcoString>,
        need_brackets: bool,
    ) {
        for class_id in symbol_map.iter_class() {
            let class = symbol_map.class(class_id);
            if exclude.contains(class.name()) {
                continue;
            }

            let arg_snippet = class
                .iter_template_arg()
                .enumerate()
                .map(|(i, _)| format!("${{{}}}", i + 1))
                .collect::<Vec<_>>()
                .join(", ");
            let snippet = if !arg_snippet.is_empty() || need_brackets {
                format!("{name}<{arg_snippet}>$0", name = class.name())
            } else {
                format!("{name}$0", name = class.name())
            };

            let define_loc = class.define_loc();
            let parse = self.db.parse(define_loc.file);
            let documentation = utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

            let mut item = CompletionItem::new_simple(
                class.name().clone(),
                "class",
                CompletionItemKind::Class,
            );
            item.insert_text_snippet = Some(snippet);
            item.documentation = documentation;
            item.item_type = Some(Type::record(class_id.into(), class.name().clone()));
            self.items.push(item);
        }
    }

    fn complete_multiclasses(&mut self, symbol_map: &SymbolMap, exclude: &HashSet<EcoString>) {
        for multiclass_id in symbol_map.iter_multiclass() {
            let multiclass = symbol_map.multiclass(multiclass_id);
            if exclude.contains(&multiclass.name) {
                continue;
            }

            let arg_snippet = multiclass
                .iter_template_arg()
                .enumerate()
                .map(|(i, _)| format!("${{{}}}", i + 1))
                .collect::<Vec<_>>()
                .join(", ");
            let snippet = format!(
                "{}{}$0",
                multiclass.name,
                if arg_snippet.is_empty() {
                    "".to_string()
                } else {
                    format!("<{arg_snippet}>")
                }
            );

            let define_loc = &multiclass.define_loc;
            let parse = self.db.parse(define_loc.file);
            let documentation = utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

            let mut item = CompletionItem::new_simple(
                multiclass.name.clone(),
                "multiclass",
                CompletionItemKind::Multiclass,
            );
            item.insert_text_snippet = Some(snippet);
            item.documentation = documentation;
            self.items.push(item);
        }
    }

    fn complete_template_arguments(&mut self, symbol_map: &SymbolMap, class_id: ClassId) {
        let class = symbol_map.class(class_id);
        for arg_id in class.iter_template_arg() {
            let arg = symbol_map.template_arg(arg_id);

            let define_loc = &arg.define_loc;
            let parse = self.db.parse(define_loc.file);
            let documentation = utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

            let mut item = CompletionItem::new_simple(
                arg.name.clone(),
                arg.typ.to_string(),
                CompletionItemKind::TemplateArgument,
            );
            item.documentation = documentation;
            item.item_type = Some(arg.typ.clone());
            self.items.push(item);
        }
    }

    fn complete_record_fields(
        &mut self,
        symbol_map: &SymbolMap,
        record_ids: &[RecordId],
        exclude_field_name: Option<EcoString>,
    ) {
        for record_id in record_ids {
            let record = symbol_map.record(*record_id);
            for field_id in record.iter_field(symbol_map) {
                let field = symbol_map.record_field(field_id);
                if let Some(ref exclude_field_name) = exclude_field_name
                    && exclude_field_name == &field.name
                {
                    continue;
                }

                let define_loc = &field.define_loc;
                let parse = self.db.parse(define_loc.file);
                let documentation =
                    utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

                let mut item = CompletionItem::new_simple(
                    field.name.clone(),
                    field.typ.to_string(),
                    CompletionItemKind::Field,
                );
                item.documentation = documentation;
                item.item_type = Some(field.typ.clone());
                self.items.push(item);
            }
        }
    }

    fn complete_defs(&mut self, symbol_map: &SymbolMap) {
        for def_id in symbol_map.iter_def() {
            let def = symbol_map.def(def_id);

            let define_loc = def.define_loc();
            let parse = self.db.parse(define_loc.file);
            let documentation = utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

            let mut item =
                CompletionItem::new_simple(def.name().clone(), "def", CompletionItemKind::Def);
            item.documentation = documentation;
            item.item_type = Some(Type::record(def_id.into(), def.name().clone()));
            self.items.push(item);
        }
    }

    fn complete_defsets(&mut self, symbol_map: &SymbolMap) {
        for defset_id in symbol_map.iter_defset() {
            let defset = symbol_map.defset(defset_id);

            let define_loc = &defset.define_loc;
            let parse = self.db.parse(define_loc.file);
            let documentation = utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

            let mut item = CompletionItem::new_simple(
                defset.name.clone(),
                defset.typ.to_string(),
                CompletionItemKind::Defset,
            );
            item.documentation = documentation;
            item.item_type = Some(defset.typ.clone());
            self.items.push(item);
        }
    }

    fn complete_variables(
        &mut self,
        _symbol_map: &SymbolMap,
        variables: Vec<(EcoString, Option<Type>)>,
    ) {
        for (name, typ) in variables {
            let detail = typ.as_ref().map(|t| t.to_string()).unwrap_or_default();
            let mut item = CompletionItem::new_simple(name, detail, CompletionItemKind::Variable);
            item.item_type = typ;
            self.items.push(item);
        }
    }
}

fn collect_variables_from_ancestors(node: &SyntaxNode) -> Vec<(EcoString, Option<Type>)> {
    let mut variables = Vec::new();
    let mut seen_names = HashSet::new();

    // foreach can be nested, so we need to collect variables from each scope
    for ancestor in node.ancestors() {
        if let Some(foreach) = ast::Foreach::cast(ancestor.clone()) {
            if let Some(iterator) = foreach.iterator() {
                if let Some(name) = iterator.name().and_then(|n| n.value()) {
                    if seen_names.insert(name.clone()) {
                        variables.push((name, None));
                    }
                }
            }
        }
    }

    // collect defvar from record bodies (class/def)
    for ancestor in node.ancestors() {
        if let Some(body) = ast::Body::cast(ancestor.clone()) {
            for item in body.items() {
                if let ast::BodyItem::Defvar(defvar) = item {
                    if let Some(name) = defvar.name().and_then(|n| n.value()) {
                        let defvar_range = defvar.syntax().text_range();
                        let node_range = node.text_range();
                        if defvar_range.end() <= node_range.start() {
                            if seen_names.insert(name.clone()) {
                                variables.push((name, None));
                            }
                        }
                    }
                }
            }

            // Stop at the first Body we encounter (current record body scope)
            break;
        }
    }

    // statement lists also can be nested, collect defvar from them too
    for ancestor in node.ancestors() {
        if let Some(stmt_list) = ast::StatementList::cast(ancestor.clone()) {
            for stmt in stmt_list.statements() {
                if let ast::Statement::Defvar(defvar) = stmt {
                    if let Some(name) = defvar.name().and_then(|n| n.value()) {
                        // Only add if it's defined before the current position
                        let defvar_range = defvar.syntax().text_range();
                        let node_range = node.text_range();
                        if defvar_range.end() <= node_range.start() {
                            if seen_names.insert(name.clone()) {
                                variables.push((name, None));
                            }
                        }
                    }
                }
            }
        }
    }

    variables
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::CompletionItem;

    fn check(s: &str) -> Vec<CompletionItem> {
        let (db, f) = tests::single_file(s);
        let mut result =
            super::exec(&db, f.source_unit_id(), f.marker(0), None).expect("completion failed");
        result.sort_by(|a, b| a.sort_text.cmp(&b.sort_text).then(a.label.cmp(&b.label)));
        result
    }

    fn check_trigger(s: &str, trigger_char: impl Into<String>) -> Vec<CompletionItem> {
        let (db, f) = tests::single_file(s);
        let mut result = super::exec(
            &db,
            f.source_unit_id(),
            f.marker(0),
            Some(trigger_char.into()),
        )
        .expect("completion failed");
        result.sort_by(|a, b| a.label.cmp(&b.label));
        result
    }

    #[test]
    fn type_matching_priority() {
        let result = check(r#"defvar var1 = 1; defvar var2 = "hoge"; class Foo { int field1 = v$"#);
        println!("{:?}", result);
        let idx_var1 = result.iter().position(|it| it.label == "var1").unwrap();
        let idx_var2 = result.iter().position(|it| it.label == "var2").unwrap();
        assert!(idx_var1 < idx_var2);

        let result = check(r#"defvar var1 = "hoge"; defvar var2 = 1; class Foo { int field1 = v$"#);
        println!("{:?}", result);
        let idx_var1 = result.iter().position(|it| it.label == "var1").unwrap();
        let idx_var2 = result.iter().position(|it| it.label == "var2").unwrap();
        assert!(idx_var1 < idx_var2);
    }

    #[test]
    fn keyword() {
        insta::assert_debug_snapshot!(check("c$"));
    }

    #[test]
    fn r#type() {
        insta::assert_debug_snapshot!(check("class Foo<i$"));
        insta::assert_debug_snapshot!(check("class Foo; class Bar<F$"));

        insta::assert_debug_snapshot!(check("class Foo<int a>; class Bar : F$;"));
        insta::assert_debug_snapshot!(check(
            "class Base1; class Base2; class Derived : Base1, B$;"
        ));
    }

    #[test]
    fn value() {
        insta::assert_debug_snapshot!(check("class Foo<int a = t$"));
        insta::assert_debug_snapshot!(check("def foo; defvar tmp = f$"));
        insta::assert_debug_snapshot!(check(
            "class Foo; defset list<Foo> foo = {} defvar tmp = f$"
        ));

        insta::assert_debug_snapshot!(check("class Foo { int field1; int field2 = f$",));
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; } class Bar : Foo { int field2 = f$",
        ));
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; } def foo : Foo { int field2 = f$",
        ));
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; } def foo : Foo { int field2 = f$ }",
        ));
        insta::assert_debug_snapshot!(check(
            r#"
        class Foo { int field1; }
        foreach i = 0...3 in {
          def foo#i : Foo { int field2 = f$ }
        }
        "#,
        ));
    }

    #[test]
    fn bang_operator() {
        insta::assert_debug_snapshot!(check_trigger("!$", "!"));
    }

    #[test]
    fn field_let() {
        insta::assert_debug_snapshot!(check("class Foo { int Bar; let B$"));
        insta::assert_debug_snapshot!(check(
            "class Foo { int Bar; int Baz; } class Foo2 : Foo { let B$"
        ));
        insta::assert_debug_snapshot!(check("class Foo { int field1; } def foo : Foo { let f$"));
    }

    #[test]
    fn class_field() {
        insta::assert_debug_snapshot!(check("class Foo { int field1; int field2 = f$"));
        insta::assert_debug_snapshot!(check(
            "class Base { int base_field; } class Derived : Base { int derived_field = b$"
        ));
    }

    #[test]
    fn multiclass() {
        insta::assert_debug_snapshot!(check("multiclass Foo; defm bar : F$"));
        insta::assert_debug_snapshot!(check(
            "multiclass Base<int x>; multiclass Derived<int y>; defm test : B$"
        ));
    }

    #[test]
    fn variables() {
        insta::assert_debug_snapshot!(check("defvar x = 10; defvar y = x$"));
        insta::assert_debug_snapshot!(check("foreach i = 0...4 in { defvar x = i$ }"));
        insta::assert_debug_snapshot!(check(
            "defvar outer = 1; foreach i = 0...2 in { defvar inner = outer$ }"
        ));
        insta::assert_debug_snapshot!(check("class Foo { defvar hoge = 1; int x = h$"));
    }

    #[test]
    fn doc_comment() {
        insta::assert_debug_snapshot!(check(
            r#"
// doc comment for class
class Foo;
class Bar : F$
            "#
        ));
        insta::assert_debug_snapshot!(check(
            r#"
// doc comment for def
def foo;
defvar tmp = f$
            "#
        ));
        insta::assert_debug_snapshot!(check(
            r#"
class Foo {
    // doc comment for field
    int field1;
}
class Bar<Foo foo> {
    int x = foo.f$
}
            "#
        ));
        insta::assert_debug_snapshot!(check(
            r#"
// doc comment for multiclass
multiclass Foo;
defm bar : F$
            "#
        ));
    }

    #[test]
    fn field_suffix() {
        // '.' trigger
        insta::assert_debug_snapshot!(check_trigger(
            "class Foo { int field1; int field2; } class Bar<Foo foo> { int x = foo.$",
            ".",
        ));
        // partial field name
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; int field2; } class Bar<Foo foo> { int x = foo.f$",
        ));
        // nested field access
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; int field2; } class Bar { Foo foo; } class Baz<Bar bar> { int x = bar.foo.f$",
        ));
        // derived class
        insta::assert_debug_snapshot!(check(
            "class Base { int base_field; } class Derived : Base { int derived_field; } class Bar<Derived bar> { int x = bar.b$"
        ));
        // list of records
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; } class Bar<list<Foo> foos> { int x = foos[0].f$"
        ));
        // list of lists of records
        insta::assert_debug_snapshot!(check(
            "class Foo { int field1; } class Bar<list<list<Foo>> foos> { int x = foos[0][0].f$"
        ));
        // anonymous class
        insta::assert_debug_snapshot!(check(
            r#"
class base1 { int field1; }
class base2 { int field2; }
class base3 { int field3; }
class derived12: base1, base2;
class derived123: base1, base2, base3;
def derived12_val : derived12;
def derived123_val : derived123;
defvar val1 = !if(false, derived12_val, derived123_val);
defvar val2 = val1.f$
            "#
        ));
    }
}
