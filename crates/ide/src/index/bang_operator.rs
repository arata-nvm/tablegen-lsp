use syntax::{
    ast::{self, AstNode},
    syntax_kind::SyntaxKind,
};

use crate::{
    TY,
    symbol_map::{
        typ::Type,
        variable::{Variable, VariableKind},
    },
};

use super::{IndexExpression, context::IndexCtx, scope::ScopeKind, utils};

impl IndexExpression for ast::BangOperator {
    type Output = Type;

    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
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
                let typ = common::expect_type_annotation(ctx, self).unwrap_or(Type::unknown());
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

                if let Some((arguments_range, Some(arguments_type))) = value_types.next()
                    && !arguments_type.is_list()
                {
                    ctx.error_by_textrange(
                        arguments_range,
                        format!("expected list, found {arguments_type}"),
                    );
                }

                if let Some((names_range, Some(names_type))) = value_types.next()
                    && !names_type.can_be_casted_to(&ctx.symbol_map, &TY![list<string>])
                {
                    ctx.error_by_textrange(
                        names_range,
                        format!("expected list<string>, found {names_type}"),
                    );
                }

                Some(TY![dag])
            }
            SyntaxKind::XEmpty => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next()
                    && !(typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                        || typ.is_list()
                        || typ.can_be_casted_to(&ctx.symbol_map, &TY![dag]))
                {
                    ctx.error_by_textrange(
                        range,
                        format!("expected string, list, or dag; found {typ}"),
                    );
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
                    if !(typ.can_be_casted_to(&ctx.symbol_map, &TY![bit])
                        || typ.is_bits()
                        || typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                        || typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                        || typ.is_record())
                    {
                        ctx.error_by_textrange(
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

                if let Some((range, Some(typ))) = value_types.next()
                    && !typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(range, format!("expected string, found {typ}"));
                }

                Some(TY![bit])
            }
            SyntaxKind::XFilter => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);

                let var = values.first()?;
                let list = values.get(1)?;
                let predicate = values.get(2)?;

                let list_typ = list.index_expression(ctx)?;
                let var_typ = if let Some(elm_typ) = list_typ.list_element_type() {
                    elm_typ.clone()
                } else {
                    ctx.error_by_syntax(list.syntax(), format!("expected list, found {list_typ}"));
                    Type::unknown()
                };

                let (var_name, var_define_loc) = match var.inner_values().next()?.simple_value() {
                    Some(ast::SimpleValue::Identifier(identifier)) => {
                        utils::identifier(&identifier, ctx)?
                    }
                    _ => return None,
                };

                let var = Variable::new(
                    var_name.clone(),
                    var_typ,
                    VariableKind::XForeach,
                    var_define_loc,
                );
                let var_id = ctx.symbol_map.add_variable(var);
                ctx.scopes.push(ScopeKind::XFilter(var_name, var_id));
                let _ = predicate.index_expression(ctx);
                ctx.scopes.pop();

                Some(list_typ)
            }
            SyntaxKind::XFind => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((string1_range, Some(string1_typ))) = value_types.next()
                    && !string1_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(
                        string1_range,
                        format!("expected string, found {string1_typ}"),
                    );
                }

                if let Some((string2_range, Some(string2_typ))) = value_types.next()
                    && !string2_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(
                        string2_range,
                        format!("expected string, found {string2_typ}"),
                    );
                }

                if let Some((start_range, Some(start_typ))) = value_types.next()
                    && !start_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                {
                    ctx.error_by_textrange(start_range, format!("expected int, found {start_typ}"));
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

                let init_typ = init.index_expression(ctx)?;
                let list_typ = list.index_expression(ctx)?;
                let list_elm_typ = if let Some(elm_typ) = list_typ.list_element_type() {
                    elm_typ.clone()
                } else {
                    ctx.error_by_syntax(list.syntax(), format!("expected list, found {list_typ}"));
                    Type::unknown()
                };

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

                let var_acc = Variable::new(
                    acc_name.clone(),
                    init_typ.clone(),
                    VariableKind::XFoldl,
                    acc_define_loc,
                );
                let var_acc_id = ctx.symbol_map.add_variable(var_acc);
                let var_var = Variable::new(
                    var_name.clone(),
                    list_elm_typ,
                    VariableKind::XFoldl,
                    var_define_loc,
                );
                let var_var_id = ctx.symbol_map.add_variable(var_var);

                ctx.scopes.push(ScopeKind::XFoldl(
                    acc_name, var_acc_id, var_name, var_var_id,
                ));
                let _ = expr.index_expression(ctx);
                ctx.scopes.pop();

                Some(init_typ)
            }
            SyntaxKind::XForEach => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);

                let var = values.first()?;
                let sequence = values.get(1)?;
                let expr = values.get(2)?;

                let sequence_typ = sequence.index_expression(ctx)?;
                let var_typ = if let Some(elm_typ) = sequence_typ.list_element_type() {
                    elm_typ.clone()
                } else {
                    ctx.error_by_syntax(
                        sequence.syntax(),
                        format!("expected list, found {sequence_typ}"),
                    );
                    Type::unknown()
                };

                let (var_name, var_define_loc) = match var.inner_values().next()?.simple_value() {
                    Some(ast::SimpleValue::Identifier(identifier)) => {
                        utils::identifier(&identifier, ctx)?
                    }
                    _ => return None,
                };

                let var = Variable::new(
                    var_name.clone(),
                    var_typ,
                    VariableKind::XForeach,
                    var_define_loc,
                );
                let var_id = ctx.symbol_map.add_variable(var);
                ctx.scopes.push(ScopeKind::XForeach(var_name, var_id));
                let expr_typ = expr.index_expression(ctx);
                ctx.scopes.pop();

                let expr_typ = expr_typ.unwrap_or(Type::unknown());
                Some(Type::list(expr_typ))
            }
            SyntaxKind::XGe | SyntaxKind::XGt | SyntaxKind::XLe | SyntaxKind::XLt => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let value_types = common::index_values(ctx, values);

                for (range, typ) in value_types.into_iter().take(2) {
                    let Some(typ) = typ else {
                        continue;
                    };
                    if !(typ.can_be_casted_to(&ctx.symbol_map, &TY![bit])
                        || typ.is_bits()
                        || typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                        || typ.can_be_casted_to(&ctx.symbol_map, &TY![string]))
                    {
                        ctx.error_by_textrange(
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

                if let Some((dag_range, Some(dag_typ))) = value_types.next()
                    && !dag_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(dag_range, format!("expected dag, found {dag_typ}"));
                }

                if let Some((key_range, Some(key_typ))) = value_types.next()
                    && !(key_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                        || key_typ.can_be_casted_to(&ctx.symbol_map, &TY![string]))
                {
                    ctx.error_by_textrange(
                        key_range,
                        format!("expected int, or string; found {key_typ}"),
                    );
                }

                Some(typ.unwrap_or(Type::unknown()))
            }
            SyntaxKind::XGetDagName => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next()
                    && !dag_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(dag_range, format!("expected dag, found {dag_typ}"));
                }

                if let Some((index_range, Some(index_typ))) = value_types.next()
                    && !index_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(index_range, format!("expected dag, found {index_typ}"));
                }

                Some(TY![string])
            }
            SyntaxKind::XGetDagOp => {
                let typ = self.r#type().and_then(|it| it.index_expression(ctx));
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next()
                    && !dag_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(dag_range, format!("expected dag, found {dag_typ}"));
                }

                Some(typ.unwrap_or(Type::unknown()))
            }
            SyntaxKind::XHead => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let (list_range, list_typ) = value_types.next()?;
                let list_typ = list_typ?;
                if let Some(elm_typ) = list_typ.list_element_type() {
                    return Some(elm_typ.clone());
                } else {
                    ctx.error_by_textrange(list_range, format!("expected list, found {list_typ}"));
                    Some(Type::unknown())
                }
            }
            SyntaxKind::XIf => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((test_range, Some(test_typ))) = value_types.next()
                    && !(test_typ.can_be_casted_to(&ctx.symbol_map, &TY![bit])
                        || test_typ.can_be_casted_to(&ctx.symbol_map, &TY![int]))
                {
                    ctx.error_by_textrange(
                        test_range,
                        format!("expected bit, or int; found {test_typ}"),
                    );
                }

                let Some((_, Some(then_typ))) = value_types.next() else {
                    return Some(Type::unknown());
                };
                let Some((else_range, Some(else_typ))) = value_types.next() else {
                    return Some(Type::unknown());
                };

                if then_typ.can_be_casted_to(&ctx.symbol_map, &else_typ) {
                    Some(then_typ)
                } else {
                    ctx.error_by_textrange(
                        else_range,
                        format!("inconsistent types {then_typ} and {else_typ} for !if"),
                    );
                    Some(Type::unknown())
                }
            }
            SyntaxKind::XInitialized => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                common::index_values(ctx, values);
                Some(TY![bit])
            }
            SyntaxKind::XInstances => {
                let typ = common::expect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 0..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((regex_range, Some(regex_typ))) = value_types.next()
                    && !regex_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(
                        regex_range,
                        "expected string type argument in !instances operator",
                    );
                }

                typ.map(Type::list)
            }
            SyntaxKind::XInterleave => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((list_range, Some(list_type))) = value_types.next() {
                    let is_not_acceptable_typ = |elm_typ: &Type| {
                        elm_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                            || elm_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                            || elm_typ.is_bits()
                            || elm_typ.can_be_casted_to(&ctx.symbol_map, &TY![bit])
                    };
                    if list_type
                        .list_element_type()
                        .map(|elm_typ| is_not_acceptable_typ(&elm_typ))
                        .unwrap_or(true)
                    {
                        ctx.error_by_textrange(
                            list_range,
                            format!(
                                "expected list of string, int, bits, or bit; found {list_type}"
                            ),
                        );
                    }
                }

                if let Some((delim_range, Some(delim_type))) = value_types.next()
                    && !delim_type.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(
                        delim_range,
                        format!("expected string, found {delim_type}"),
                    );
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
                    ctx.error_by_textrange(
                        list1_range,
                        format!("expected list, found {list1_type}"),
                    );
                    return Some(Type::unknown());
                }

                for (range, typ) in value_types {
                    let Some(typ) = typ else {
                        continue;
                    };
                    if !typ.can_be_casted_to(&ctx.symbol_map, &list1_type) {
                        ctx.error_by_textrange(
                            range,
                            format!("expected {list1_type}, found {typ}"),
                        );
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

                if let Some(elm_typ) = list_type.list_element_type() {
                    if elm_typ.is_list() {
                        Some(elm_typ.clone())
                    } else {
                        Some(Type::list(elm_typ.clone()))
                    }
                } else {
                    ctx.error_by_textrange(list_range, format!("expected list, found {list_type}"));
                    Some(Type::unknown())
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
                    ctx.error_by_textrange(
                        list1_range,
                        format!("expected list, found {list1_type}"),
                    );
                    return Some(Type::unknown());
                }

                let (list2_range, list2_type) = value_types.next()?;
                let list2_type = list2_type.clone()?;

                if !list2_type.can_be_casted_to(&ctx.symbol_map, &list1_type) {
                    ctx.error_by_textrange(
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

                if let Some((count_range, Some(count_typ))) = value_types.next()
                    && !count_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                {
                    ctx.error_by_textrange(count_range, format!("expected int, found {count_typ}"));
                }

                Some(Type::list(value_type.clone()))
            }
            SyntaxKind::XLog2 => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next()
                    && !typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                {
                    ctx.error_by_textrange(range, format!("expected int, found {typ}"));
                }

                Some(TY![int])
            }
            SyntaxKind::XNot => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next()
                    && !typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                {
                    ctx.error_by_textrange(range, format!("expected int, found {typ}"));
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

                if start_or_list_typ.can_be_casted_to(&ctx.symbol_map, &TY![int]) {
                    for (range, typ) in value_types.take(2) {
                        let Some(typ) = typ else {
                            continue;
                        };
                        if !typ.can_be_casted_to(&ctx.symbol_map, &TY![int]) {
                            ctx.error_by_textrange(range, format!("expected int, found {typ}"));
                        }
                    }
                } else if start_or_list_typ.is_list() {
                    if value_types.next().is_some() {
                        ctx.error_by_textrange(
                            start_or_list_range,
                            format!(
                                "expected one list, found extra value of type {start_or_list_typ}"
                            ),
                        );
                    }
                } else {
                    ctx.error_by_textrange(
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

                if let Some((dag_range, Some(dag_typ))) = value_types.next()
                    && !dag_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(dag_range, format!("expected dag, found {dag_typ}"));
                }

                if let Some((key_range, Some(key_typ))) = value_types.next()
                    && !(key_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                        || key_typ.can_be_casted_to(&ctx.symbol_map, &TY![string]))
                {
                    ctx.error_by_textrange(
                        key_range,
                        format!("expected int, or string; found {key_typ}"),
                    );
                }

                Some(TY![dag])
            }
            SyntaxKind::XSetDagName => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next()
                    && !dag_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(dag_range, format!("expected dag, found {dag_typ}"));
                }

                if let Some((key_range, Some(key_typ))) = value_types.next()
                    && !(key_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                        || key_typ.can_be_casted_to(&ctx.symbol_map, &TY![string]))
                {
                    ctx.error_by_textrange(
                        key_range,
                        format!("expected int, or string; found {key_typ}"),
                    );
                }

                if let Some((name_range, Some(name_typ))) = value_types.next()
                    && !name_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(
                        name_range,
                        format!("expected string, found {name_typ}"),
                    );
                }

                Some(TY![dag])
            }
            SyntaxKind::XSetDagOp => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=2);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((dag_range, Some(dag_typ))) = value_types.next()
                    && !dag_typ.can_be_casted_to(&ctx.symbol_map, &TY![dag])
                {
                    ctx.error_by_textrange(dag_range, format!("expected dag, found {dag_typ}"));
                }

                Some(TY![dag])
            }
            SyntaxKind::XSize => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next()
                    && !(typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                        || typ.is_list()
                        || typ.can_be_casted_to(&ctx.symbol_map, &TY![dag]))
                {
                    ctx.error_by_textrange(
                        range,
                        format!("expected string, list, or dag; found {typ}"),
                    );
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
                    if !typ.can_be_casted_to(&ctx.symbol_map, &TY![string]) {
                        ctx.error_by_textrange(range, format!("expected string, found {typ}"));
                    }
                }

                Some(TY![string])
            }
            SyntaxKind::XSubst => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 3..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                let (target_range, target_typ) = value_types.next()?;
                let (repl_range, repl_typ) = value_types.next()?;
                let (value_range, value_typ) = value_types.next()?;

                let value_typ = value_typ?;
                if !(value_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                    || value_typ.is_record())
                {
                    ctx.error_by_textrange(
                        value_range,
                        format!("expected string or record, found {value_typ}"),
                    );
                    return Some(Type::unknown());
                }

                let target_typ = target_typ?;
                if !target_typ.can_be_casted_to(&ctx.symbol_map, &value_typ) {
                    ctx.error_by_textrange(
                        target_range,
                        format!("expected {value_typ}, found {target_typ}"),
                    );
                }

                let repl_typ = repl_typ?;
                if !repl_typ.can_be_casted_to(&ctx.symbol_map, &value_typ) {
                    ctx.error_by_textrange(
                        repl_range,
                        format!("expected {value_typ}, found {repl_typ}"),
                    );
                }

                Some(value_typ)
            }
            SyntaxKind::XSubstr => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 2..=3);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((string_range, Some(string_typ))) = value_types.next()
                    && !string_typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(
                        string_range,
                        format!("expected string, found {string_typ}"),
                    );
                }

                if let Some((start_range, Some(start_typ))) = value_types.next()
                    && !start_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                {
                    ctx.error_by_textrange(start_range, format!("expected int, found {start_typ}"));
                }

                if let Some((len_range, Some(len_typ))) = value_types.next()
                    && !len_typ.can_be_casted_to(&ctx.symbol_map, &TY![int])
                {
                    ctx.error_by_textrange(len_range, format!("expected int, found {len_typ}"));
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

                    ctx.error_by_textrange(list_range, format!("expected list, found {list_typ}"));
                }
                Some(Type::unknown())
            }
            SyntaxKind::XToLower | SyntaxKind::XToUpper => {
                common::unexpect_type_annotation(ctx, self);
                let values = common::expect_values(ctx, self, 1..=1);
                let mut value_types = common::index_values(ctx, values).into_iter();

                if let Some((range, Some(typ))) = value_types.next()
                    && !typ.can_be_casted_to(&ctx.symbol_map, &TY![string])
                {
                    ctx.error_by_textrange(range, format!("expected string, found {typ}"));
                }
                Some(TY![string])
            }
            _ => {
                tracing::warn!("unexpected syntax kind: {:?}", self.kind());
                Some(Type::unknown())
            }
        }
    }
}

mod common {
    use std::ops::{Bound, RangeBounds};

    use syntax::{
        ast::{self, AstNode},
        parser::TextRange,
    };

    use crate::{index::IndexExpression, symbol_map::typ::Type};

    pub(super) fn expect_type_annotation(
        ctx: &mut super::IndexCtx,
        node: &ast::BangOperator,
    ) -> Option<Type> {
        match node.r#type() {
            Some(typ) => typ.index_expression(ctx),
            None => {
                ctx.error_by_syntax(node.syntax(), "expected type annotation");
                None
            }
        }
    }

    pub(super) fn unexpect_type_annotation(ctx: &mut super::IndexCtx, node: &ast::BangOperator) {
        if let Some(typ) = node.r#type() {
            ctx.error_by_syntax(typ.syntax(), "unexpected type annotation");
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
                    ctx.error_by_syntax(
                        node.syntax(),
                        format!("expected {start} arguments, found {values_len}"),
                    );
                }
            }
            (Bound::Included(start), Bound::Included(end)) if start != end => {
                if values.len() < *start || *end < values.len() {
                    ctx.error_by_syntax(
                        node.syntax(),
                        format!("expected {start} to {end} arguments, found {values_len}"),
                    );
                }
            }
            (Bound::Included(start), Bound::Unbounded) => {
                if values.len() < *start {
                    ctx.error_by_syntax(
                        node.syntax(),
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
            .map(|value| (value.syntax().text_range(), value.index_expression(ctx)))
            .collect()
    }

    pub(super) fn index_values_and_check_types(
        ctx: &mut super::IndexCtx,
        values: Vec<ast::Value>,
        expected: &Type,
    ) {
        for value in values {
            let Some(value_type) = value.index_expression(ctx) else {
                continue;
            };

            if !value_type.can_be_casted_to(&ctx.symbol_map, expected) {
                ctx.error_by_syntax(
                    value.syntax(),
                    format!("expected {expected}, found {value_type}"),
                );
            }
        }
    }
}
