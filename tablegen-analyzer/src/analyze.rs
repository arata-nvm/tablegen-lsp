use ecow::EcoString;
use tablegen_parser::{
    ast::{self, AstNode},
    error::SyntaxError,
    language::SyntaxNode,
    parser::TextRange,
};

use crate::{
    document::DocumentId,
    indexer::DocumentIndexer,
    symbol::{RecordFieldType, RecordKind},
    symbol_map::SymbolMap,
};

pub fn analyze(doc_id: DocumentId, root: SyntaxNode) -> (SymbolMap, Vec<SyntaxError>) {
    let mut indexer = DocumentIndexer::new(doc_id);
    analyze_root(root, &mut indexer);
    indexer.finish()
}

fn analyze_root(root: SyntaxNode, i: &mut DocumentIndexer) {
    let root = ast::Root::cast(root);
    with(
        root.and_then(|root| root.statement_list()),
        |statement_list| {
            analyze_statement_list(statement_list, i);
        },
    );
}

fn analyze_statement_list(list: ast::StatementList, i: &mut DocumentIndexer) {
    for stmt in list.statements() {
        match stmt {
            ast::Statement::Class(class) => analyze_class(class, i),
            ast::Statement::Def(def) => analyze_def(def, i),
            _ => {}
        }
    }
}

fn analyze_class(class: ast::Class, i: &mut DocumentIndexer) {
    with_id(class.name(), |name, range| {
        let symbol_id = i.add_record(name, range, RecordKind::Class);

        i.push(symbol_id);
        with(class.template_arg_list(), |list| {
            for arg in list.args() {
                analyze_template_arg(arg, i);
            }
        });
        with(class.record_body(), |record_body| {
            analyze_record_body(record_body, i);
        });
        i.pop();

        Some(())
    });
}

fn analyze_template_arg(arg: ast::TemplateArgDecl, i: &mut DocumentIndexer) {
    with_id(arg.name(), |name, range| {
        let typ = analyze_type(arg.r#type()?, i)?;
        i.add_template_arg(name, range, typ);
        Some(())
    });
}

fn analyze_record_body(record_body: ast::RecordBody, i: &mut DocumentIndexer) {
    with(record_body.parent_class_list(), |list| {
        for class_ref in list.classes() {
            analyze_class_ref(class_ref, i);
        }
    });
    with(record_body.body(), |body| {
        analyze_body(body, i);
    });
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
            _ => unimplemented!(),
        }
    }
}

fn analyze_field_def(field_def: ast::FieldDef, i: &mut DocumentIndexer) {
    with_id(field_def.name(), |name, range| {
        let typ = analyze_type(field_def.r#type()?, i)?;
        i.add_field(name, range, typ);

        with(field_def.value(), |value| {
            analyze_value(value, i);
        });

        Some(())
    });
}

fn analyze_field_let(field_let: ast::FieldLet, i: &mut DocumentIndexer) {
    with(field_let.value(), |value| {
        analyze_value(value, i);
    });
}

fn analyze_def(def: ast::Def, i: &mut DocumentIndexer) {
    let Some(name) = def.name() else { return; };
    let Some((name, range)) =  analyze_name_value(name) else { return; };
    let symbol_id = i.add_record(name, range, RecordKind::Def);

    i.push(symbol_id);
    with(def.record_body(), |record_body| {
        analyze_record_body(record_body, i);
    });
    i.pop();
}

fn analyze_name_value(value: ast::Value) -> Option<(EcoString, TextRange)> {
    let name = value.inner_values().next()?;
    match name.simple_value()? {
        ast::SimpleValue::Identifier(id) => with_id(Some(id), |name, range| Some((name, range))),
        _ => None,
    }
}

fn analyze_type(typ: ast::Type, i: &mut DocumentIndexer) -> Option<RecordFieldType> {
    let typ = match typ {
        ast::Type::BitType(_) => RecordFieldType::Bit,
        ast::Type::IntType(_) => RecordFieldType::Int,
        ast::Type::StringType(_) => RecordFieldType::String,
        ast::Type::DagType(_) => RecordFieldType::Dag,
        ast::Type::BitsType(bits_typ) => {
            let len = bits_typ.length()?.value()?;
            RecordFieldType::Bits(len)
        }
        ast::Type::ListType(list_typ) => {
            let inner_typ = analyze_type(list_typ.inner_type()?, i)?;
            RecordFieldType::List(Box::new(inner_typ))
        }
        ast::Type::ClassId(class_id) => with_id(class_id.name(), |name, range| {
            let symbol_id = i.add_symbol_reference(name.clone(), range)?;
            Some(RecordFieldType::Class(symbol_id, name))
        })?,
        ast::Type::CodeType(_) => RecordFieldType::Code,
    };
    Some(typ)
}

fn analyze_class_ref(class_ref: ast::ClassRef, i: &mut DocumentIndexer) {
    with_id(class_ref.name(), |name, range| {
        i.add_symbol_reference(name, range)
    });
    with(class_ref.arg_value_list(), |list| {
        with(list.positional(), |positional| {
            for arg in positional.values() {
                analyze_value(arg, i);
            }
        });
    });
}

fn analyze_value(value: ast::Value, i: &mut DocumentIndexer) -> Option<()> {
    let inner_value = value.inner_values().next()?;
    let simple_value = inner_value.simple_value()?;
    let ast::SimpleValue::Identifier(id) = simple_value else { return None; };
    let symbol_id = with_id(Some(id), |name, range| i.add_symbol_reference(name, range))?;

    let suffix = inner_value.suffixes().next()?;
    let ast::ValueSuffix::FieldSuffix(field_name) = suffix else { return None; };
    with_id(field_name.name(), |name, range| {
        i.access_field(symbol_id, name.clone(), range)
    });

    None
}

fn with<T>(t: Option<T>, f: impl FnOnce(T)) {
    if let Some(t) = t {
        f(t);
    }
}

fn with_id<T>(
    id: Option<ast::Identifier>,
    f: impl FnOnce(EcoString, TextRange) -> Option<T>,
) -> Option<T> {
    let Some(id) = id else { return None; };
    let Some(name) = id.value() else { return None; };
    let range = id.syntax().text_range();
    f(name, range)
}
