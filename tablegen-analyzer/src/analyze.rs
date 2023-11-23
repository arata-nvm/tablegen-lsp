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
    symbol::{RecordKind, Symbol, SymbolType, VariableKind},
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
    with_id(field_let.name(), |name, range| {
        let symbol_id = i.scope_symbol_id();
        let field_id = i.access_field(symbol_id, name.clone(), range)?;
        let field = i.symbol(field_id)?.as_field()?;
        i.add_field(name, range, field.r#type().clone());
        Some(())
    });
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
            let typ = analyze_value(value, i);
            i.add_variable(name, range, VariableKind::Defvar, typ);
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
        let symbol_id = i.add_symbol_reference(name, range)?;
        i.add_parent(symbol_id);
        Some(())
    });
    with(class_ref.arg_value_list(), |list| {
        with(list.positional(), |positional| {
            for arg in positional.values() {
                analyze_value(arg, i);
            }
        });
    });
}

fn analyze_value(value: ast::Value, i: &mut DocumentIndexer) -> SymbolType {
    let mut values = value.inner_values();

    let first_value = values.next();
    let first_value_typ = first_value.map(|inner_value| analyze_inner_value(inner_value, i));
    for inner_value in values {
        analyze_inner_value(inner_value, i);
    }

    match value.inner_values().count() {
        0 => None,
        1 => first_value_typ,
        _ => Some(SymbolType::String),
    }
    .unwrap_or(SymbolType::unknown())
}

fn analyze_inner_value(inner_value: ast::InnerValue, i: &mut DocumentIndexer) -> SymbolType {
    let Some(simple_value) = inner_value.simple_value() else { return SymbolType::unknown(); };

    let mut lhs_typ: SymbolType = analyze_simple_value(simple_value, i);
    for suffix in inner_value.suffixes() {
        let suffixed_type = match suffix {
            ast::ValueSuffix::RangeSuffix(range_suffix) => {
                analyze_range_suffix(range_suffix, i);
                match lhs_typ {
                    SymbolType::Bits(_) | SymbolType::Int => Some(SymbolType::Int),
                    _ => None,
                }
            }
            ast::ValueSuffix::SliceSuffix(slice_suffix) => {
                let is_single_element = slice_suffix.is_single_element();
                analyze_slice_suffix(slice_suffix, i);
                if is_single_element {
                    Some(lhs_typ.element_typ().unwrap_or(SymbolType::unknown()))
                } else {
                    Some(lhs_typ)
                }
            }
            ast::ValueSuffix::FieldSuffix(field_suffix) => match lhs_typ {
                SymbolType::Class(symbol_id, _) => with_id(field_suffix.name(), |name, range| {
                    let field_symbol_id = i.access_field(symbol_id, name, range)?;
                    let field = i.symbol(field_symbol_id)?;
                    Some(field.as_field()?.r#type().clone())
                }),
                _ => None,
            },
        };
        lhs_typ = suffixed_type.unwrap_or(SymbolType::unknown());
    }
    lhs_typ
}

fn analyze_range_suffix(range_suffix: ast::RangeSuffix, i: &mut DocumentIndexer) {
    with(range_suffix.range_list(), |list| {
        for piece in list.pieces() {
            with(piece.start(), |value| {
                analyze_value(value, i);
            });
            with(piece.end(), |value| {
                analyze_value(value, i);
            });
        }
    });
}

fn analyze_slice_suffix(slice_suffix: ast::SliceSuffix, i: &mut DocumentIndexer) {
    with(slice_suffix.element_list(), |list| {
        for element in list.elements() {
            with(element.start(), |value| {
                analyze_value(value, i);
            });
            with(element.end(), |value| {
                analyze_value(value, i);
            });
        }
    });
}

fn analyze_simple_value(simple_value: ast::SimpleValue, i: &mut DocumentIndexer) -> SymbolType {
    match simple_value {
        ast::SimpleValue::Integer(_) => SymbolType::Int,
        ast::SimpleValue::String(_) => SymbolType::String,
        ast::SimpleValue::Code(_) => SymbolType::Code,
        ast::SimpleValue::Boolean(_) => SymbolType::Bit,
        ast::SimpleValue::Uninitialized(_) => SymbolType::unknown(),
        ast::SimpleValue::Bits(bits) => {
            let Some(list) = bits.value_list() else { return SymbolType::Bits(0) };
            for value in list.values() {
                analyze_value(value, i);
            }
            SymbolType::Bits(list.values().count() as i64)
        }
        ast::SimpleValue::List(list) => {
            let Some(list) = list.value_list() else { return SymbolType::List(Box::new(SymbolType::unknown())) };
            let value_typs: Vec<SymbolType> =
                list.values().map(|value| analyze_value(value, i)).collect();
            let first_value_typ = value_typs.get(0).cloned().unwrap_or(SymbolType::unknown());
            SymbolType::List(Box::new(first_value_typ.clone()))
        }
        ast::SimpleValue::Dag(dag) => {
            with(dag.arg_list(), |list| {
                for arg in list.args() {
                    with(arg.value(), |value| {
                        analyze_value(value, i);
                    });
                }
            });
            SymbolType::Dag
        }
        ast::SimpleValue::Identifier(identifier) => with_id(Some(identifier), |name, range| {
            let symbol_id = i.add_symbol_reference(name, range)?;
            match i.symbol(symbol_id)? {
                Symbol::Record(record)
                    if matches!(
                        record.kind(),
                        RecordKind::Def | RecordKind::Defset | RecordKind::Defvar
                    ) =>
                {
                    Some(SymbolType::Class(symbol_id, record.name().clone()))
                }
                Symbol::RecordField(field) => Some(field.r#type().clone()),
                Symbol::Variable(variable) => Some(variable.r#type().clone()),
                _ => None,
            }
        })
        .unwrap_or(SymbolType::unknown()),
        ast::SimpleValue::ClassValue(class_value) => {
            with(class_value.arg_value_list(), |list| {
                with(list.positional(), |positional| {
                    for value in positional.values() {
                        analyze_value(value, i);
                    }
                })
            });
            with_id(class_value.name(), |name, range| {
                i.add_symbol_reference(name.clone(), range)
                    .map(|symbol_id| SymbolType::Class(symbol_id, name))
            })
            .unwrap_or(SymbolType::unknown())
        }
        ast::SimpleValue::BangOperator(bang_op) => {
            analyze_bang_operator(bang_op, i).unwrap_or(SymbolType::unknown())
        }
        ast::SimpleValue::CondOperator(cond_op) => {
            let clauses: Vec<SymbolType> = cond_op
                .clauses()
                .map(|clause| {
                    with(clause.condition(), |value| {
                        analyze_value(value, i);
                    });
                    let value_typ = clause.value().map(|value| analyze_value(value, i));
                    value_typ.unwrap_or(SymbolType::unknown())
                })
                .collect();
            clauses.get(0).cloned().unwrap_or(SymbolType::unknown())
        }
    }
}

fn analyze_bang_operator(
    bang_op: ast::BangOperator,
    i: &mut DocumentIndexer,
) -> Option<SymbolType> {
    fn check_num_args(kind: BangOperator, bang_op: &ast::BangOperator, i: &mut DocumentIndexer) {
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
    ) -> Option<SymbolType> {
        let arg_var = arg_var.inner_values().nth(0)?;
        let ast::SimpleValue::Identifier(arg_var) =  arg_var.simple_value()? else { return None; };

        let arg_list_range = arg_list.syntax().text_range();
        let arg_list_typ = analyze_value(arg_list, i);
        let elm_typ = match arg_list_typ {
            SymbolType::List(ref elm_typ) => *elm_typ.clone(),
            SymbolType::Unresolved(_) => SymbolType::unknown(),
            _ => {
                i.error(arg_list_range, "!filter must have a list argument");
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

        Some(arg_list_typ)
    }

    fn check_xforeach(
        arg_var: ast::Value,
        arg_sequence: ast::Value,
        arg_expr: ast::Value,
        i: &mut DocumentIndexer,
    ) -> Option<SymbolType> {
        let arg_var = arg_var.inner_values().nth(0)?;
        let ast::SimpleValue::Identifier(arg_var) =  arg_var.simple_value()? else { return None; };

        let arg_sequence_range = arg_sequence.syntax().text_range();
        let arg_sequence_typ = analyze_value(arg_sequence, i);
        let elm_typ = match arg_sequence_typ {
            SymbolType::List(elm_typ) => *elm_typ,
            SymbolType::Dag => SymbolType::unknown(),
            SymbolType::Unresolved(_) => SymbolType::unknown(),
            _ => {
                i.error(
                    arg_sequence_range,
                    "!foreach must have a list or dag argument",
                );
                SymbolType::unknown()
            }
        };

        with_id(Some(arg_var), |name, range: TextRange| {
            i.push_temporary();
            i.add_temporary_variable(name, range, elm_typ);
            let arg_expr_typ = analyze_value(arg_expr, i);
            i.pop_temporary();
            Some(arg_expr_typ)
        })
    }

    fn check_xfoldl(
        arg_init: ast::Value,
        arg_list: ast::Value,
        arg_acc: ast::Value,
        arg_var: ast::Value,
        arg_expr: ast::Value,
        i: &mut DocumentIndexer,
    ) -> Option<SymbolType> {
        let arg_acc = arg_acc.inner_values().nth(0)?;
        let ast::SimpleValue::Identifier(arg_acc) =  arg_acc.simple_value()? else { return None; };

        let arg_var = arg_var.inner_values().nth(0)?;
        let ast::SimpleValue::Identifier(arg_var) =  arg_var.simple_value()? else { return None; };

        let arg_init_typ = analyze_value(arg_init, i);
        let arg_list_range = arg_list.syntax().text_range();
        let arg_list_typ = analyze_value(arg_list, i);
        let elm_typ = match arg_list_typ {
            SymbolType::List(elm_typ) => *elm_typ,
            SymbolType::Unresolved(_) => SymbolType::unknown(),
            x => {
                i.error(
                    arg_list_range,
                    eco_format!("!foldl list must be a list, but is of type '{x}'"),
                );
                SymbolType::unknown()
            }
        };

        with_id(Some(arg_acc), |name_acc, range_acc| {
            with_id(Some(arg_var), |name_var, range_var| {
                i.push_temporary();
                i.add_temporary_variable(name_acc, range_acc, elm_typ.clone());
                i.add_temporary_variable(name_var, range_var, elm_typ);
                analyze_value(arg_expr, i);
                i.pop_temporary();
                Some(())
            })
        });

        Some(arg_init_typ)
    }

    let kind: BangOperator = bang_op.kind()?.try_into().ok()?;
    check_num_args(kind, &bang_op, i);

    use BangOperator::*;
    let mut values = bang_op.values();
    match kind {
        XFilter => return check_xfilter(values.next()?, values.next()?, values.next()?, i),
        XFoldl => {
            return check_xfoldl(
                values.next()?,
                values.next()?,
                values.next()?,
                values.next()?,
                values.next()?,
                i,
            )
        }
        XForEach => return check_xforeach(values.next()?, values.next()?, values.next()?, i),
        _ => {}
    }

    let value_types: Vec<SymbolType> = values.map(|value| analyze_value(value, i)).collect();
    let typ = match kind {
        XAdd => SymbolType::Int,
        XAnd => SymbolType::Bit,
        XCast => analyze_type(bang_op.r#type()?, i)?,
        XConcat => SymbolType::Dag,
        XDag => SymbolType::Dag,
        XDiv => SymbolType::Int,
        XEmpty => SymbolType::Bit,
        XEq => SymbolType::Bit,
        XExists => SymbolType::Bit,
        XFind => SymbolType::Int,
        XGe => SymbolType::Bit,
        XGetDagArg => analyze_type(bang_op.r#type()?, i)?,
        XGetDagName => SymbolType::String,
        XGetDagOp => analyze_type(bang_op.r#type()?, i)?,
        XGt => SymbolType::Bit,
        XHead => value_types.get(0)?.element_typ()?,
        XIf => value_types.get(1)?.clone(),
        XInterleave => SymbolType::String, // TODO
        XIsA => SymbolType::Bit,
        XLe => SymbolType::Bit,
        XListConcat => value_types.get(0)?.clone(),
        XListRemove => value_types.get(0)?.clone(),
        XListSplat => SymbolType::List(Box::new(value_types.get(0)?.clone())),
        XLog2 => SymbolType::Int,
        XLt => SymbolType::Bit,
        XMul => SymbolType::Int,
        XNe => SymbolType::Bit,
        XNot => SymbolType::Bit,
        XOr => SymbolType::Bit,
        XRange => SymbolType::List(Box::new(SymbolType::Int)),
        XSetDagArg => SymbolType::Dag,
        XSetDagName => SymbolType::Dag,
        XSetDagOp => SymbolType::Dag,
        XShl => SymbolType::Int,
        XSize => SymbolType::String, // TODO
        XSra => SymbolType::Int,
        XSrl => SymbolType::Int,
        XStrConcat => SymbolType::String,
        XSub => SymbolType::Int,
        XSubst => SymbolType::String,
        XSubstr => SymbolType::String,
        XTail => value_types.get(0)?.element_typ()?,
        XToLower => SymbolType::String,
        XToUpper => SymbolType::String,
        XXor => SymbolType::Bit,
        _ => unreachable!(),
    };
    Some(typ)
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
