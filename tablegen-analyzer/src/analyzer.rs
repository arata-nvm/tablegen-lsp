use ecow::EcoString;
use tablegen_parser::{
    ast,
    error::{Range, SyntaxError},
    node::SyntaxNode,
};

use crate::{document::DocumentId, indexer::DocumentIndexer, symbol_map::SymbolMap};

pub fn analyze(doc_id: DocumentId, file: &SyntaxNode) -> (SymbolMap, Vec<SyntaxError>) {
    let mut indexer = DocumentIndexer::new(doc_id);
    analyze_file(file, &mut indexer);
    indexer.finish()
}

fn analyze_file(file: &SyntaxNode, i: &mut DocumentIndexer) -> Option<()> {
    let file = file.cast::<ast::File>()?;
    let list = file.statement_list()?;
    for stmt in list.statements() {
        match stmt {
            ast::Statement::Class(class) => analyze_class(class, i),
            ast::Statement::Def(def) => analyze_def(def, i),
            _ => {}
        }
    }
    None
}

fn analyze_class(class: ast::Class, i: &mut DocumentIndexer) {
    let Some(symbol_id) = with_id(class.name(), |name, range| {
        i.add_record(name, range)
    }) else { return; };

    i.push(symbol_id);
    if let Some(template_arg_list) = class.template_arg_list() {
        for arg in template_arg_list.args() {
            analyze_template_arg(arg, i);
        }
    }
    if let Some(record_body) = class.record_body() {
        analyze_record_body(record_body, i);
    }
    i.pop();
}

fn analyze_template_arg(arg: ast::TemplateArgDecl, i: &mut DocumentIndexer) {
    with_id(arg.name(), |name, range| i.add_template_arg(name, range));
    if let Some(typ) = arg.r#type() {
        analyze_type(typ, i);
    }
}

fn analyze_record_body(record_body: ast::RecordBody, i: &mut DocumentIndexer) {
    if let Some(parent_class_list) = record_body.parent_class_list() {
        for class_ref in parent_class_list.classes() {
            analyze_class_ref(class_ref, i);
        }
    }
    if let Some(body) = record_body.body() {
        analyze_body(body, i);
    }
}

fn analyze_body(body: ast::Body, i: &mut DocumentIndexer) {
    for item in body.items() {
        match item {
            ast::BodyItem::FieldDef(field_def) => {
                analyze_field_def(field_def, i);
            }
            ast::BodyItem::FieldLet(field_let) => {
                analyze_field_let(field_let, i);
            }
        }
    }
}

fn analyze_field_def(field_def: ast::FieldDef, i: &mut DocumentIndexer) {
    if let Some(typ) = field_def.r#type() {
        analyze_type(typ, i);
    }
    with_id(field_def.name(), |name, range| i.add_field(name, range));
    if let Some(value) = field_def.value() {
        analyze_value(value, i);
    }
}

fn analyze_field_let(field_let: ast::FieldLet, i: &mut DocumentIndexer) {
    if let Some(value) = field_let.value() {
        analyze_value(value, i);
    }
}

fn analyze_def(def: ast::Def, i: &mut DocumentIndexer) {
    let Some(name) = def.name() else { return; };
    let Some(ast::SimpleValue::Identifier(id)) = name.simple_value() else { return; };
    let Some(symbol_id) = with_id(Some(id), |name, range| {
        i.add_record(name, range)
    }) else { return; };

    i.push(symbol_id);
    if let Some(record_body) = def.record_body() {
        analyze_record_body(record_body, i);
    }
    i.pop();
}

fn analyze_type(typ: ast::Type, i: &mut DocumentIndexer) -> Option<()> {
    match typ {
        ast::Type::ListType(list_typ) => {
            analyze_type(list_typ.inner_type()?, i);
        }
        ast::Type::ClassId(class_id) => {
            with_id(class_id.name(), |name, range| {
                i.add_symbol_reference(name, range)
            });
        }
        _ => {}
    }
    None
}

fn analyze_class_ref(class_ref: ast::ClassRef, i: &mut DocumentIndexer) {
    with_id(class_ref.name(), |name, range| {
        i.add_symbol_reference(name, range)
    });
    if let Some(arg_value_list) = class_ref.arg_value_list() {
        if let Some(positional) = arg_value_list.positional() {
            for arg in positional.values() {
                analyze_value(arg, i);
            }
        }
    }
}

fn analyze_value(value: ast::Value, i: &mut DocumentIndexer) -> Option<()> {
    let simple_value = value.simple_value()?;
    match simple_value {
        ast::SimpleValue::Identifier(id) => {
            with_id(Some(id), |name, range| i.add_symbol_reference(name, range));
        }
        _ => {}
    }
    None
}

fn with_id<T>(id: Option<ast::Identifier>, f: impl FnOnce(&EcoString, Range) -> T) -> Option<T> {
    let Some(id) = id else { return None; };
    let Some(name) = id.value() else { return None; };
    let range = id.range();
    Some(f(name, range))
}
