use ecow::{eco_format, EcoString};
use tablegen_parser::{
    ast::{self, AstNode},
    bang_operator::BangOperator,
    error::SyntaxError,
    language::SyntaxNode,
    parser::TextRange,
};

use crate::{
    document::DocumentId,
    indexer::DocumentIndexer,
    symbol::{RecordKind, SymbolType, VariableKind},
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
            ast::Statement::Defset(defset) => analyze_defset(defset, i),
            ast::Statement::Defvar(defvar) => analyze_defvar(defvar, i),
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
            _ => {}
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

fn analyze_defset(defset: ast::Defset, i: &mut DocumentIndexer) {
    with_id(defset.name(), |name, range| {
        with(defset.r#type(), |typ| {
            let Some(typ) = analyze_type(typ, i) else { return; };
            i.add_variable(name, range, VariableKind::Defset, typ);
        });
        with(defset.statement_list(), |list| {
            analyze_statement_list(list, i);
        });
        Some(())
    });
}

fn analyze_defvar(defvar: ast::Defvar, i: &mut DocumentIndexer) {
    with_id(defvar.name(), |name, range| {
        with(defvar.value(), |value| {
            let typ = infer_type(value.clone(), i);
            i.add_variable(name, range, VariableKind::Defvar, typ);

            analyze_value(value, i);
        });
        Some(())
    });
}

fn analyze_type(typ: ast::Type, i: &mut DocumentIndexer) -> Option<SymbolType> {
    let typ = match typ {
        ast::Type::BitType(_) => SymbolType::Bit,
        ast::Type::IntType(_) => SymbolType::Int,
        ast::Type::StringType(_) => SymbolType::String,
        ast::Type::DagType(_) => SymbolType::Dag,
        ast::Type::BitsType(bits_typ) => {
            let len = bits_typ.length()?.value()?;
            SymbolType::Bits(len)
        }
        ast::Type::ListType(list_typ) => {
            let inner_typ = analyze_type(list_typ.inner_type()?, i)?;
            SymbolType::List(Box::new(inner_typ))
        }
        ast::Type::ClassId(class_id) => with_id(class_id.name(), |name, range| {
            match i.add_symbol_reference(name.clone(), range) {
                Some(symbol_id) => Some(SymbolType::Class(symbol_id, name)),
                None => Some(SymbolType::Unresolved(name.clone())),
            }
        })?,
        ast::Type::CodeType(_) => SymbolType::Code,
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

fn analyze_value(value: ast::Value, i: &mut DocumentIndexer) {
    for inner_value in value.inner_values() {
        analyze_inner_value(inner_value, i);
    }

    // let suffix = inner_value.suffixes().next()?;
    // let ast::ValueSuffix::FieldSuffix(field_name) = suffix else { return None; };
    // with_id(field_name.name(), |name, range| {
    //     i.access_field(symbol_id, name.clone(), range)
    // });

    // None
}

fn analyze_inner_value(inner_value: ast::InnerValue, i: &mut DocumentIndexer) {
    with(inner_value.simple_value(), |simple_value| {
        analyze_simple_value(simple_value, i);
    });
}

fn analyze_simple_value(simple_value: ast::SimpleValue, i: &mut DocumentIndexer) {
    match simple_value {
        ast::SimpleValue::Integer(_)
        | ast::SimpleValue::String(_)
        | ast::SimpleValue::Code(_)
        | ast::SimpleValue::Boolean(_)
        | ast::SimpleValue::Uninitialized(_) => {}
        ast::SimpleValue::Bits(bits) => with(bits.value_list(), |list| {
            for value in list.values() {
                analyze_value(value, i);
            }
        }),
        ast::SimpleValue::List(list) => with(list.value_list(), |list| {
            for value in list.values() {
                analyze_value(value, i);
            }
        }),
        ast::SimpleValue::Dag(dag) => {
            with(dag.arg_list(), |list| {
                for arg in list.args() {
                    with(arg.value(), |value| analyze_value(value, i));
                }
            });
        }
        ast::SimpleValue::Identifier(identifier) => {
            with_id(Some(identifier), |name, range| {
                i.add_symbol_reference(name, range)
            });
        }
        ast::SimpleValue::ClassRef(class_ref) => {
            with_id(class_ref.name(), |name, range| {
                i.add_symbol_reference(name, range)
            });
            with(
                class_ref
                    .arg_value_list()
                    .and_then(|list| list.positional()),
                |list| {
                    for value in list.values() {
                        analyze_value(value, i);
                    }
                },
            );
        }
        ast::SimpleValue::BangOperator(bang_op) => analyze_bang_operator(bang_op, i),
        ast::SimpleValue::CondOperator(cond_op) => {
            for clause in cond_op.clauses() {
                with(clause.condition(), |value| analyze_value(value, i));
                with(clause.value(), |value| analyze_value(value, i));
            }
        }
    };
}

fn analyze_bang_operator(bang_op: ast::BangOperator, i: &mut DocumentIndexer) {
    fn check_args(kind: BangOperator, bang_op: &ast::BangOperator, i: &mut DocumentIndexer) {
        let num_actual = bang_op.values().count();
        if !kind.is_valid_num_of_args(num_actual) {
            let (min, max) = (kind.min_num_of_args(), kind.max_num_of_args());
            let num_expect = if min == max {
                eco_format!("{min}")
            } else {
                eco_format!("{min}-{max}")
            };
            let msg = eco_format!("{kind} expects {num_expect} arguments, but got {num_actual}",);
            i.error(bang_op.syntax().text_range(), msg);
        }
    }

    fn check_xfilter(
        arg_var: ast::Value,
        arg_list: ast::Value,
        arg_predicate: ast::Value,
        i: &mut DocumentIndexer,
    ) {
        analyze_value(arg_list.clone(), i);

        let Some(arg_var) = arg_var.inner_values().nth(0) else { return; };
        let Some(ast::SimpleValue::Identifier(arg_var)) =  arg_var.simple_value() else { return; };

        let arg_list_typ = infer_type(arg_list.clone(), i);
        let elm_typ = match arg_list_typ {
            SymbolType::List(elm_typ) => *elm_typ,
            SymbolType::Unresolved(_) => SymbolType::unknown(),
            _ => {
                i.error(
                    arg_list.syntax().text_range(),
                    "!filter must have a list argument",
                );
                SymbolType::unknown()
            }
        };

        with_id(Some(arg_var), |name, range: TextRange| {
            i.push_temporary();
            i.add_temporary_variable(name, range, elm_typ);
            analyze_value(arg_predicate, i);
            i.pop_temporary();
            Some(())
        });
    }

    fn check_xforeach(
        arg_var: ast::Value,
        arg_sequence: ast::Value,
        arg_expr: ast::Value,
        i: &mut DocumentIndexer,
    ) {
        analyze_value(arg_sequence.clone(), i);

        let Some(arg_var) = arg_var.inner_values().nth(0) else { return; };
        let Some(ast::SimpleValue::Identifier(arg_var)) =  arg_var.simple_value() else { return; };

        let arg_sequence_typ = infer_type(arg_sequence.clone(), i);
        let elm_typ = match arg_sequence_typ {
            SymbolType::List(elm_typ) => *elm_typ,
            SymbolType::Dag => SymbolType::unknown(),
            SymbolType::Unresolved(_) => SymbolType::unknown(),
            _ => {
                i.error(
                    arg_sequence.syntax().text_range(),
                    "!foreach must have a list or dag argument",
                );
                SymbolType::unknown()
            }
        };

        with_id(Some(arg_var), |name, range: TextRange| {
            i.push_temporary();
            i.add_temporary_variable(name, range, elm_typ);
            analyze_value(arg_expr, i);
            i.pop_temporary();
            Some(())
        });
    }

    with(
        bang_op.kind().and_then(|kind| kind.try_into().ok()),
        |kind: BangOperator| {
            check_args(kind, &bang_op, i);
            let mut values = bang_op.values();
            match kind {
                BangOperator::XFilter => {
                    let Some(arg_var) = values.next() else { return; };
                    let Some(arg_list) = values.next() else { return; };
                    let Some(arg_predicate) = values.next() else { return; };
                    check_xfilter(arg_var, arg_list, arg_predicate, i);
                }
                BangOperator::XForEach => {
                    let Some(arg_var) = values.next() else { return; };
                    let Some(arg_sequence) = values.next() else { return; };
                    let Some(arg_expr) = values.next() else { return; };
                    check_xforeach(arg_var, arg_sequence, arg_expr, i);
                }
                _ => {
                    for value in values {
                        analyze_value(value, i);
                    }
                }
            }
        },
    );
}

fn infer_type(value: ast::Value, i: &mut DocumentIndexer) -> SymbolType {
    let simple_value = value
        .inner_values()
        .nth(0)
        .and_then(|inner_value| inner_value.simple_value());
    if let Some(ast::SimpleValue::Identifier(identifier)) = simple_value {
        let symbol_id = with_id(Some(identifier), |name, range| {
            i.add_symbol_reference(name, range)
        });
        if let Some(symbol) = symbol_id.and_then(|symbol_id| i.symbol(symbol_id)) {
            if let Some(typ) = symbol.as_variable().map(|variable| variable.r#type()) {
                return typ.clone();
            }
        };
    }

    SymbolType::unknown()
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
