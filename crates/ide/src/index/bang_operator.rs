use syntax::{ast, syntax_kind::SyntaxKind};

use crate::{
    symbol_map::{
        typ::Type,
        variable::{Variable, VariableKind},
    },
    TY,
};

use super::{context::IndexCtx, scope::ScopeKind, utils, Indexable};
// Optionを返すインターフェイスのために、Type::Unknownをデフォルトとする処理を誰が行うかが不明瞭
// 型が解決できなかった場合にSome(Type::Unknown)を返すが、その呼び出し元でunwrap_or(Type::Unknown)を
// 呼び出しているかもしれない
impl Indexable for ast::BangOperator {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        // BangOperatorの種類をenumで表現したい
        match self.kind()? {
            SyntaxKind::XAdd
            | SyntaxKind::XAnd
            | SyntaxKind::XMul
            | SyntaxKind::XOr
            | SyntaxKind::XXor => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..);
                common::index_values_and_check_types(ctx, values, &TY![int]);
                Some(TY![int])
            }
            SyntaxKind::XDiv
            | SyntaxKind::XSub
            | SyntaxKind::XSrl
            | SyntaxKind::XSra
            | SyntaxKind::XShl => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                common::index_values_and_check_types(ctx, values, &TY![int]);
                Some(TY![int])
            }
            SyntaxKind::XCast => {
                let typ = common::expect_type_annotation(ctx, self).unwrap_or(Type::Unknown);
                let values = common::expect_values(ctx, self, 1..=1);
                common::index_values(ctx, values);
                Some(typ)
            }
            SyntaxKind::XCon => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..);
                common::index_values_and_check_types(ctx, values, &TY![dag]);
                Some(TY![dag])
            }
            SyntaxKind::XDag => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let _ = value_types.next();

                if let Some((arguments_range, Some(arguments_type))) = value_types.next() {
                    if !arguments_type.is_list() {
                        ctx.error(
                            arguments_range,
                            format!("expected list, found {arguments_type}"),
                        );
                    }
                }

                if let Some((names_range, Some(names_type))) = value_types.next() {
                    if !names_type.isa(&ctx.symbol_map, &TY![list<string>]) {
                        ctx.error(
                            names_range,
                            format!("expected list<string>, found {names_type}"),
                        );
                    }
                }

                Some(TY![dag])
            }
            SyntaxKind::XEmpty => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next() {
                    if !(typ.isa(&ctx.symbol_map, &TY![string])
                        || typ.is_list()
                        || typ.isa(&ctx.symbol_map, &TY![dag]))
                    {
                        ctx.error(range, format!("expected string, list, or dag; found {typ}"));
                    }
                }

                Some(TY![bit])
            }
            SyntaxKind::XEq | SyntaxKind::XNe => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let value_types = common::index_values(ctx, values).into_iter();

                for (range, typ) in value_types.take(2) {
                    let Some(typ) = typ else {
                        continue;
                    };
                    if !(typ.isa(&ctx.symbol_map, &TY![bit])
                        || typ.is_bits()
                        || typ.isa(&ctx.symbol_map, &TY![int])
                        || typ.isa(&ctx.symbol_map, &TY![string])
                        || typ.is_record())
                    {
                        ctx.error(
                            range,
                            format!("expected bit, bits, int, string, or record; found {typ}",),
                        );
                    }
                }

                Some(TY![bit])
            }
            SyntaxKind::XExists => {
                common::expect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next() {
                    if !typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(range, format!("expected string, found {typ}"));
                    }
                }

                Some(TY![bit])
            }
            SyntaxKind::XFilter => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);

                let var = values.first()?;
                let list = values.get(1)?;
                let predicate = values.get(2)?;

                let list_typ = list.index(ctx)?;
                let var_typ = list_typ.element_typ()?;

                let (var_name, var_define_loc) = match var.inner_values().next()?.simple_value() {
                    Some(ast::SimpleValue::Identifier(identifier)) => {
                        utils::identifier(&identifier, ctx)?
                    }
                    _ => return None,
                };

                ctx.scopes.push(ScopeKind::XFilter);
                let variable =
                    Variable::new(var_name, var_typ, VariableKind::XForeach, var_define_loc);
                ctx.scopes.add_variable(&mut ctx.symbol_map, variable);
                predicate.index(ctx);
                ctx.scopes.pop();

                Some(list_typ)
            }
            SyntaxKind::XFind => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((string1_range, Some(string1_typ))) = value_types.next() {
                    if !string1_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(
                            string1_range,
                            format!("expected string, found {string1_typ}"),
                        );
                    }
                }

                if let Some((string2_range, Some(string2_typ))) = value_types.next() {
                    if !string2_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(
                            string2_range,
                            format!("expected string, found {string2_typ}"),
                        );
                    }
                }

                if let Some((start_range, Some(start_typ))) = value_types.next() {
                    if !start_typ.isa(&ctx.symbol_map, &TY![int]) {
                        ctx.error(start_range, format!("expected int, found {start_typ}"));
                    }
                }

                Some(TY![int])
            }
            SyntaxKind::XFoldl => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 5..=5);

                let init = values.first()?;
                let list = values.get(1)?;
                let acc = values.get(2)?;
                let var = values.get(3)?;
                let expr = values.get(4)?;

                let init_typ = init.index(ctx)?;
                let _list_typ = list.index(ctx)?;

                let (acc_name, acc_define_loc) = match acc.inner_values().next()?.simple_value() {
                    Some(ast::SimpleValue::Identifier(identifier)) => {
                        utils::identifier(&identifier, ctx)?
                    }
                    _ => return None,
                };
                let (var_name, var_define_loc) = match var.inner_values().next()?.simple_value() {
                    Some(ast::SimpleValue::Identifier(identifier)) => {
                        utils::identifier(&identifier, ctx)?
                    }
                    _ => return None,
                };

                ctx.scopes.push(ScopeKind::XFoldl);
                let variable_acc = Variable::new(
                    acc_name,
                    init_typ.clone(),
                    VariableKind::XFoldl,
                    acc_define_loc,
                );
                ctx.scopes.add_variable(&mut ctx.symbol_map, variable_acc);
                let variable_var = Variable::new(
                    var_name,
                    init_typ.clone(),
                    VariableKind::XFoldl,
                    var_define_loc,
                );
                ctx.scopes.add_variable(&mut ctx.symbol_map, variable_var);
                expr.index(ctx);
                ctx.scopes.pop();

                Some(init_typ)
            }
            SyntaxKind::XForEach => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);

                let var = values.first()?;
                let sequence = values.get(1)?;
                let expr = values.get(2)?;

                let sequence_typ = sequence.index(ctx)?;
                let var_typ = sequence_typ.element_typ()?;

                let (var_name, var_define_loc) = match var.inner_values().next()?.simple_value() {
                    Some(ast::SimpleValue::Identifier(identifier)) => {
                        utils::identifier(&identifier, ctx)?
                    }
                    _ => return None,
                };

                ctx.scopes.push(ScopeKind::XForeach);
                let variable =
                    Variable::new(var_name, var_typ, VariableKind::XForeach, var_define_loc);
                ctx.scopes.add_variable(&mut ctx.symbol_map, variable);
                let expr_typ = expr.index(ctx);
                ctx.scopes.pop();

                let expr_typ = expr_typ.unwrap_or(Type::Unknown);
                Some(Type::List(Box::new(expr_typ)))
            }
            SyntaxKind::XGe | SyntaxKind::XGt | SyntaxKind::XLe | SyntaxKind::XLt => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let value_types = common::index_values(ctx, values);

                for (range, typ) in value_types.into_iter().take(2) {
                    let Some(typ) = typ else {
                        continue;
                    };
                    if !(typ.isa(&ctx.symbol_map, &TY![bit])
                        || typ.is_bits()
                        || typ.isa(&ctx.symbol_map, &TY![int])
                        || typ.isa(&ctx.symbol_map, &TY![string]))
                    {
                        ctx.error(
                            range,
                            format!("expected bit, bits, int, or string; found {typ}",),
                        );
                    }
                }

                Some(TY![bit])
            }
            SyntaxKind::XGetDagArg => {
                let typ = common::expect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next() {
                    if !dag_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(dag_range, format!("expected dag, found {dag_typ}"));
                    }
                }

                if let Some((key_range, Some(key_typ))) = value_types.next() {
                    if !(key_typ.isa(&ctx.symbol_map, &TY![int])
                        || key_typ.isa(&ctx.symbol_map, &TY![string]))
                    {
                        ctx.error(
                            key_range,
                            format!("expected int, or string; found {key_typ}"),
                        );
                    }
                }

                Some(typ.unwrap_or(Type::Unknown))
            }
            SyntaxKind::XGetDagName => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next() {
                    if !dag_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(dag_range, format!("expected dag, found {dag_typ}"));
                    }
                }

                if let Some((index_range, Some(index_typ))) = value_types.next() {
                    if !index_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(index_range, format!("expected dag, found {index_typ}"));
                    }
                }

                Some(TY![string])
            }
            SyntaxKind::XGetDagOp => {
                let typ = self.r#type().and_then(|it| it.index(ctx));
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next() {
                    if !dag_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(dag_range, format!("expected dag, found {dag_typ}"));
                    }
                }

                Some(typ.unwrap_or(Type::Unknown))
            }
            SyntaxKind::XHead => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let (list_range, list_typ) = value_types.next()?;
                match list_typ? {
                    Type::List(elm_typ) => Some(*elm_typ.clone()),
                    list_typ => {
                        ctx.error(list_range, format!("expected list, found {list_typ}"));
                        Some(Type::Unknown)
                    }
                }
            }
            SyntaxKind::XIf => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((test_range, Some(test_typ))) = value_types.next() {
                    if !(test_typ.isa(&ctx.symbol_map, &TY![bit])
                        || test_typ.isa(&ctx.symbol_map, &TY![int]))
                    {
                        ctx.error(
                            test_range,
                            format!("expected bit, or int; found {test_typ}"),
                        );
                    }
                }

                let Some((_, Some(then_typ))) = value_types.next() else {
                    return Some(Type::Unknown);
                };
                let Some((else_range, Some(else_typ))) = value_types.next() else {
                    return Some(Type::Unknown);
                };

                if then_typ.isa(&ctx.symbol_map, &else_typ) {
                    Some(then_typ)
                } else {
                    ctx.error(
                        else_range,
                        format!("inconsistent types {then_typ} and {else_typ} for !if"),
                    );
                    Some(Type::Unknown)
                }
            }
            SyntaxKind::XInitialized => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                common::index_values(ctx, values);
                Some(TY![bit])
            }
            SyntaxKind::XInterleave => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((list_range, Some(list_type))) = value_types.next() {
                    match list_type {
                        Type::List(elm_typ)
                            if matches!(
                                *elm_typ.clone(),
                                Type::Any | Type::String | Type::Int | Type::Bits(_) | Type::Bit
                            ) => {}
                        _ => {
                            ctx.error(
                                list_range,
                                format!(
                                    "expected list of string, int, bits, or bit; found {list_type}"
                                ),
                            );
                        }
                    }
                }

                if let Some((delim_range, Some(delim_type))) = value_types.next() {
                    if !delim_type.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(delim_range, format!("expected string, found {delim_type}"));
                    }
                }

                Some(TY![string])
            }
            SyntaxKind::XIsA => {
                common::expect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                common::index_values(ctx, values);
                Some(TY![bit])
            }
            SyntaxKind::XListConcat => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let Some((list1_range, Some(list1_type))) = value_types.next() else {
                    return None;
                };

                if !list1_type.is_list() {
                    ctx.error(list1_range, format!("expected list, found {list1_type}"));
                    return Some(Type::Unknown);
                }

                for (range, typ) in value_types {
                    let Some(typ) = typ else {
                        continue;
                    };
                    if !typ.isa(&ctx.symbol_map, &list1_type) {
                        ctx.error(range, format!("expected {list1_type}, found {typ}"));
                    }
                }

                Some(list1_type.clone())
            }
            SyntaxKind::XListFlatten => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let Some((list_range, Some(list_type))) = value_types.next() else {
                    return None;
                };

                if let Type::List(inner_type) = list_type {
                    if let Type::List(_) = *inner_type {
                        Some(*inner_type.clone())
                    } else {
                        Some(Type::List(inner_type.clone()))
                    }
                } else {
                    ctx.error(list_range, format!("expected list, found {list_type}"));
                    Some(Type::Unknown)
                }
            }
            SyntaxKind::XListRemove => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let Some((list1_range, Some(list1_type))) = value_types.next() else {
                    return None;
                };

                if !list1_type.is_list() {
                    ctx.error(list1_range, format!("expected list, found {list1_type}"));
                    return Some(Type::Unknown);
                }

                let (list2_range, list2_type) = value_types.next()?;
                let list2_type = list2_type.clone()?;

                if !list2_type.isa(&ctx.symbol_map, &list1_type) {
                    ctx.error(
                        list2_range,
                        format!("expected {list1_type}, found {list2_type}"),
                    );
                }

                Some(list1_type.clone())
            }
            SyntaxKind::XListSplat => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let Some((_, Some(value_type))) = value_types.next() else {
                    return None;
                };

                if let Some((count_range, Some(count_typ))) = value_types.next() {
                    if !count_typ.isa(&ctx.symbol_map, &TY![int]) {
                        ctx.error(count_range, format!("expected int, found {count_typ}"));
                    }
                }

                Some(Type::List(Box::new(value_type.clone())))
            }
            SyntaxKind::XLog2 => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next() {
                    if !typ.isa(&ctx.symbol_map, &TY![int]) {
                        ctx.error(range, format!("expected int, found {typ}"));
                    }
                }

                Some(TY![int])
            }
            SyntaxKind::XNot => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next() {
                    if !typ.isa(&ctx.symbol_map, &TY![int]) {
                        ctx.error(range, format!("expected int, found {typ}"));
                    }
                }

                Some(TY![bit])
            }
            SyntaxKind::XRange => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let ret_typ = Some(TY![list<int>]);

                let Some((start_or_list_range, Some(start_or_list_typ))) = value_types.next()
                else {
                    return ret_typ;
                };

                if start_or_list_typ.isa(&ctx.symbol_map, &TY![int]) {
                    for (range, typ) in value_types.take(2) {
                        let Some(typ) = typ else {
                            continue;
                        };
                        if !typ.isa(&ctx.symbol_map, &TY![int]) {
                            ctx.error(range, format!("expected int, found {typ}"));
                        }
                    }
                } else if start_or_list_typ.is_list() {
                    if value_types.next().is_some() {
                        ctx.error(
                            start_or_list_range,
                            format!(
                                "expected one list, found extra value of type {start_or_list_typ}"
                            ),
                        );
                    }
                } else {
                    ctx.error(
                        start_or_list_range,
                        format!("expected int or list, found {start_or_list_typ}"),
                    );
                }

                ret_typ
            }
            SyntaxKind::XRepr => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                common::index_values(ctx, values);
                Some(TY![string])
            }
            SyntaxKind::XSetDagArg => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next() {
                    if !dag_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(dag_range, format!("expected dag, found {dag_typ}"));
                    }
                }

                if let Some((key_range, Some(key_typ))) = value_types.next() {
                    if !(key_typ.isa(&ctx.symbol_map, &TY![int])
                        || key_typ.isa(&ctx.symbol_map, &TY![string]))
                    {
                        ctx.error(
                            key_range,
                            format!("expected int, or string; found {key_typ}"),
                        );
                    }
                }

                Some(TY![dag])
            }
            SyntaxKind::XSetDagName => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next() {
                    if !dag_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(dag_range, format!("expected dag, found {dag_typ}"));
                    }
                }

                if let Some((key_range, Some(key_typ))) = value_types.next() {
                    if !(key_typ.isa(&ctx.symbol_map, &TY![int])
                        || key_typ.isa(&ctx.symbol_map, &TY![string]))
                    {
                        ctx.error(
                            key_range,
                            format!("expected int, or string; found {key_typ}"),
                        );
                    }
                }

                if let Some((name_range, Some(name_typ))) = value_types.next() {
                    if !name_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(name_range, format!("expected string, found {name_typ}"));
                    }
                }

                Some(TY![dag])
            }
            SyntaxKind::XSetDagOp => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next() {
                    if !dag_typ.isa(&ctx.symbol_map, &TY![dag]) {
                        ctx.error(dag_range, format!("expected dag, found {dag_typ}"));
                    }
                }

                Some(TY![dag])
            }
            SyntaxKind::XSize => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next() {
                    if !(typ.isa(&ctx.symbol_map, &TY![string])
                        || typ.is_list()
                        || typ.isa(&ctx.symbol_map, &TY![dag]))
                    {
                        ctx.error(range, format!("expected string, list, or dag; found {typ}"));
                    }
                }

                Some(TY![int])
            }
            SyntaxKind::XStrConcat => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..);
                let value_types = common::index_values(ctx, values).into_iter();

                for (range, typ) in value_types {
                    let Some(typ) = typ else {
                        continue;
                    };
                    if !typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(range, format!("expected string, found {typ}"));
                    }
                }

                Some(TY![string])
            }
            SyntaxKind::XSubst => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((target_range, Some(target_typ))) = value_types.next() {
                    if !target_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(target_range, format!("expected string, found {target_typ}"));
                    }
                }

                if let Some((repl_range, Some(repl_typ))) = value_types.next() {
                    if !repl_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(repl_range, format!("expected string, found {repl_typ}"));
                    }
                }

                if let Some((value_range, Some(value_typ))) = value_types.next() {
                    if !value_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(value_range, format!("expected string, found {value_typ}"));
                    }
                }

                Some(TY![string])
            }
            SyntaxKind::XSubstr => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((string_range, Some(string_typ))) = value_types.next() {
                    if !string_typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(string_range, format!("expected string, found {string_typ}"));
                    }
                }

                if let Some((start_range, Some(start_typ))) = value_types.next() {
                    if !start_typ.isa(&ctx.symbol_map, &TY![int]) {
                        ctx.error(start_range, format!("expected int, found {start_typ}"));
                    }
                }

                if let Some((len_range, Some(len_typ))) = value_types.next() {
                    if !len_typ.isa(&ctx.symbol_map, &TY![int]) {
                        ctx.error(len_range, format!("expected int, found {len_typ}"));
                    }
                }

                Some(TY![string])
            }
            SyntaxKind::XTail => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((list_range, Some(list_typ))) = value_types.next() {
                    if list_typ.is_list() {
                        return Some(list_typ);
                    }

                    ctx.error(list_range, format!("expected list, found {list_typ}"));
                }
                Some(Type::Unknown)
            }
            SyntaxKind::XToLower | SyntaxKind::XToUpper => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next() {
                    if !typ.isa(&ctx.symbol_map, &TY![string]) {
                        ctx.error(range, format!("expected string, found {typ}"));
                    }
                }
                Some(TY![string])
            }
            _ => unreachable!("unexpected syntax kind: {:?}", self.kind()),
        }
    }
}

mod common {
    use std::ops::{Bound, RangeBounds};

    use syntax::{
        ast::{self, AstNode},
        parser::TextRange,
    };

    use crate::{index::Indexable, symbol_map::typ::Type};

    pub(super) fn expect_type_annotation(
        ctx: &mut super::IndexCtx,
        node: &ast::BangOperator,
    ) -> Option<Type> {
        match node.r#type() {
            Some(typ) => typ.index(ctx),
            None => {
                ctx.error(node.syntax().text_range(), "expected type annotation");
                None
            }
        }
    }

    pub(super) fn unexpect_type_annotation(ctx: &mut super::IndexCtx, node: &ast::BangOperator) {
        if let Some(typ) = node.r#type() {
            ctx.error(typ.syntax().text_range(), "unexpected type annotation");
        }
    }

    pub(super) fn expect_values(
        ctx: &mut super::IndexCtx,
        node: &ast::BangOperator,
        num: impl RangeBounds<usize>,
    ) -> Vec<ast::Value> {
        let values: Vec<ast::Value> = node.values().collect();
        let values_len = values.len();
        match (num.start_bound(), num.end_bound()) {
            (Bound::Included(start), Bound::Included(end)) if start == end => {
                if values.len() != *start {
                    ctx.error(
                        node.syntax().text_range(),
                        format!("expected {start} arguments, found {values_len}"),
                    );
                }
            }
            (Bound::Included(start), Bound::Included(end)) if start != end => {
                if values.len() < *start || *end < values.len() {
                    ctx.error(
                        node.syntax().text_range(),
                        format!("expected {start} to {end} arguments, found {values_len}"),
                    );
                }
            }
            (Bound::Included(start), Bound::Unbounded) => {
                if values.len() < *start {
                    ctx.error(
                        node.syntax().text_range(),
                        format!("expected {start} or more arguments, found {values_len}"),
                    );
                }
            }
            _ => unimplemented!(),
        }
        values
    }

    pub(super) fn index_values(
        ctx: &mut super::IndexCtx,
        values: Vec<ast::Value>,
    ) -> Vec<(TextRange, Option<Type>)> {
        values
            .into_iter()
            .map(|value| (value.syntax().text_range(), value.index(ctx)))
            .collect()
    }

    pub(super) fn index_values_and_check_types(
        ctx: &mut super::IndexCtx,
        values: Vec<ast::Value>,
        expected: &Type,
    ) {
        for value in values {
            let Some(value_type) = value.index(ctx) else {
                continue;
            };

            if !value_type.isa(&ctx.symbol_map, expected) {
                ctx.error(
                    value.syntax().text_range(),
                    format!("expected {expected}, found {value_type}"),
                );
            }
        }
    }
}
