use std::sync::Arc;

use ecow::EcoString;

use syntax::ast::AstNode;
use syntax::ast::{self};
use syntax::parser::TextRange;
use syntax::SyntaxNodePtr;

use crate::db::SourceDatabase;
use crate::file_system::{FileRange, IncludeId};
use crate::handlers::diagnostics::Diagnostic;
use crate::symbol_map::expr::{BangOperatorOp, DagArg, Expr, SimpleExpr};
use crate::symbol_map::field::Field;
use crate::symbol_map::record::Record;
use crate::symbol_map::symbol::{Symbol, SymbolId};
use crate::symbol_map::template_arg::TemplateArgument;
use crate::symbol_map::typ::Type;
use crate::symbol_map::value::{DagArgValue, Value};
use crate::symbol_map::variable::Variable;
use crate::symbol_map::SymbolMap;

use self::context::EvalCtx;
use self::scope::ScopeKind;

pub mod context;
pub mod scope;

#[salsa::query_group(EvalDatabaseStorage)]
pub trait EvalDatabase: SourceDatabase {
    fn eval(&self) -> Arc<Evaluation>;
}

#[derive(Debug, Eq, PartialEq)]
pub struct Evaluation {
    symbol_map: SymbolMap,
    diagnostics: Vec<Diagnostic>,
}

impl Evaluation {
    pub fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

fn eval(db: &dyn EvalDatabase) -> Arc<Evaluation> {
    let source_root = db.source_root();

    let parse = db.parse(source_root.root());
    let source_file =
        ast::SourceFile::cast(parse.syntax_node()).expect("failed to SourceFile::cast");

    let mut ctx = EvalCtx::new(db, source_root.root());
    source_file.eval(&mut ctx);
    Arc::new(ctx.finish())
}

pub trait Eval {
    type Output;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output>;
}

#[derive(Debug, Clone, Copy)]
pub enum EvalValueMode {
    AsIdentifier,
    AsValue,
}

pub trait EvalValue {
    type Output;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output>;
}

pub trait EvalExpr {
    fn eval_expr(self, ctx: &mut EvalCtx) -> Option<Value>;
}

impl Eval for ast::SourceFile {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.statement_list()?.eval(ctx);
        Some(())
    }
}

impl Eval for ast::StatementList {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        for stmt in self.statements() {
            stmt.eval(ctx);
        }
        Some(())
    }
}

impl Eval for ast::Statement {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        match self {
            ast::Statement::Class(class) => class.eval(ctx),
            ast::Statement::Def(def) => def.eval(ctx),
            ast::Statement::Include(include) => include.eval(ctx),
            ast::Statement::Defvar(defvar) => defvar.eval(ctx),
            ast::Statement::Assert(assert) => assert.eval(ctx),
            ast::Statement::If(r#if) => r#if.eval(ctx),
            _ => {
                ctx.error(
                    self.syntax().text_range(),
                    format!("{}:{} not implemented", file!(), line!()),
                );
                Some(())
            }
        }
    }
}

impl Eval for ast::Class {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let record = Record::new(name.clone(), define_loc);
        let id = ctx.symbol_map.add_class(record.clone());

        ctx.scopes.push(ScopeKind::Class(id, record));
        if let Some(list) = self.template_arg_list() {
            list.eval(ctx);
        }
        if let Some(body) = self.record_body() {
            body.eval(ctx);
        }
        let (_, record) = ctx.scopes.pop().into_class();
        ctx.symbol_map.replace_class(id, record);

        Some(())
    }
}

impl Eval for ast::Def {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = match self.name() {
            Some(name_node) => {
                let name_range = name_node.syntax().text_range();
                let define_loc = FileRange::new(ctx.current_file_id(), name_range);

                let name = self
                    .name()?
                    .eval_value(ctx, EvalValueMode::AsIdentifier)?
                    .eval_expr(ctx)?;
                let Value::String(name) = name else {
                    ctx.error(
                        define_loc.range,
                        format!("'{name}' cannot be used as an identifier"),
                    );
                    return None;
                };

                (name, define_loc)
            }
            None => {
                let name = ctx.next_anonymous_def_name();
                let name_range = TextRange::empty(self.syntax().text_range().start());
                let define_loc = FileRange::new(ctx.current_file_id(), name_range);
                (name, define_loc)
            }
        };

        let record = Record::new(name.clone(), define_loc);
        let def = record.clone().into_def(ctx);
        let id = ctx.symbol_map.add_def(def);

        ctx.scopes.push(ScopeKind::Def(id, record));
        if let Some(body) = self.record_body() {
            body.eval(ctx);
        }
        let (_, record) = ctx.scopes.pop().into_def();
        let def = record.clone().into_def(ctx);
        ctx.symbol_map.replace_def(id, def);

        Some(())
    }
}

impl Eval for ast::Defvar {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let value = self
            .value()?
            .eval_value(ctx, EvalValueMode::AsValue)?
            .eval_expr(ctx)?;
        let variable = Variable::new(name.clone(), value, define_loc);
        if ctx.resolve_id(&name).is_some() {
            ctx.error(
                define_loc.range,
                "def or global variable of this name already exists",
            );
            return None;
        }
        ctx.scopes.add_variable(&mut ctx.symbol_map, variable);
        Some(())
    }
}

impl Eval for ast::Assert {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let condition_node = self.condition()?;
        let condition_range = condition_node.syntax().text_range();
        let condition = condition_node
            .eval_value(ctx, EvalValueMode::AsValue)?
            .eval_expr(ctx)?;

        let message_value = self
            .message()?
            .eval_value(ctx, EvalValueMode::AsValue)?
            .eval_expr(ctx)?;
        let message = match message_value {
            Value::String(value) => value,
            _ => EcoString::from("(assert message is not a string)"),
        };

        match condition.cast_to(&ctx.symbol_map, &Type::Bit) {
            Some(Value::Bit(false)) => {
                ctx.error(
                    condition_range,
                    format!("assertion failed\nnote: {message}"),
                );
            }
            Some(Value::Bit(true)) => {}
            _ => {
                ctx.error(
                    condition_range,
                    "assert condition must of type bit, bits, or int.",
                );
            }
        }

        Some(())
    }
}

impl Eval for ast::If {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let cond_value = self
            .condition()?
            .eval_value(ctx, EvalValueMode::AsValue)?
            .eval_expr(ctx)?;
        match cond_value.cast_to(&ctx.symbol_map, &Type::Bit) {
            Some(Value::Bit(true)) => {
                self.then_body()?.eval(ctx);
            }
            Some(Value::Bit(false)) => {
                self.else_body()?.eval(ctx);
            }
            _ => {
                ctx.error(
                    self.condition()?.syntax().text_range(),
                    "if condition must of type bit, bits, or int.",
                );
            }
        }
        Some(())
    }
}

impl Eval for ast::TemplateArgList {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        for arg in self.args() {
            arg.eval(ctx);
        }
        Some(())
    }
}

impl Eval for ast::TemplateArgDecl {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let typ = self.r#type()?.eval(ctx)?;
        let default_value = self
            .value()
            .and_then(|it| it.eval_value(ctx, EvalValueMode::AsValue));
        let template_arg = TemplateArgument::new(name.clone(), typ, default_value, define_loc);
        ctx.scopes
            .current_record_mut()
            .add_template_arg(&mut ctx.symbol_map, template_arg);
        Some(())
    }
}

impl Eval for ast::RecordBody {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.parent_class_list().and_then(|it| it.eval(ctx));
        self.body().and_then(|it| it.eval(ctx));
        Some(())
    }
}

impl Eval for ast::ParentClassList {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        for class in self.classes() {
            class.eval(ctx);
        }
        Some(())
    }
}

impl Eval for ast::ClassRef {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, reference_loc) = utils::identifier(self.name()?, ctx)?;
        let Some(class_id) = ctx.symbol_map.find_class(&name) else {
            ctx.error(reference_loc.range, format!("class not found: {name}"));
            return None;
        };
        let arg_value_list = self
            .arg_value_list()
            .and_then(|it| it.eval(ctx))
            .unwrap_or_default();

        if let Err((range, err)) = ctx.scopes.current_record_mut().inherit(
            &mut ctx.symbol_map,
            class_id,
            arg_value_list,
            reference_loc,
        ) {
            ctx.error(range, err.to_string());
        }

        Some(())
    }
}

impl Eval for ast::ArgValueList {
    type Output = Vec<Expr>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let positional_arg_value_list = self.positional().and_then(|it| it.eval(ctx));
        if self.named().is_some() {
            ctx.error(
                self.syntax().text_range(),
                format!("{}:{} not implemented", file!(), line!()),
            );
        }
        positional_arg_value_list
    }
}

impl Eval for ast::PositionalArgValueList {
    type Output = Vec<Expr>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        Some(
            self.values()
                .filter_map(|it| it.eval_value(ctx, EvalValueMode::AsValue))
                .collect(),
        )
    }
}

impl Eval for ast::Body {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        for item in self.items() {
            item.eval(ctx);
        }
        Some(())
    }
}

impl Eval for ast::BodyItem {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        match self {
            ast::BodyItem::FieldDef(field_def) => field_def.eval(ctx),
            ast::BodyItem::FieldLet(field_let) => field_let.eval(ctx),
            _ => {
                ctx.error(
                    self.syntax().text_range(),
                    format!("{}:{} not implemented", file!(), line!()),
                );
                Some(())
            }
        }
    }
}

impl Eval for ast::FieldDef {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let typ = self.r#type()?.eval(ctx)?;
        let value = self
            .value()
            .and_then(|it| it.eval_value(ctx, EvalValueMode::AsValue))
            .unwrap_or(Expr::uninitialized(define_loc));
        let field = Field::new(
            name.clone(),
            typ,
            value,
            ctx.scopes.current_record_id(),
            define_loc,
        );

        if let Err((range, err)) = ctx
            .scopes
            .current_record_mut()
            .add_field(&mut ctx.symbol_map, field)
        {
            ctx.error(range, err.to_string());
        }

        Some(())
    }
}

impl Eval for ast::FieldLet {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let value = self
            .value()
            .and_then(|it| it.eval_value(ctx, EvalValueMode::AsValue))
            .unwrap_or(Expr::uninitialized(define_loc));

        let Some(old_field_id) = ctx.scopes.current_record().find_field(&name) else {
            ctx.error(define_loc.range, format!("unknown field: {name}"));
            return None;
        };
        let old_field = ctx.symbol_map.field(old_field_id);

        let new_field = old_field.modified(value, ctx.scopes.current_record_id(), define_loc);
        if let Err((range, err)) = ctx
            .scopes
            .current_record_mut()
            .add_field(&mut ctx.symbol_map, new_field)
        {
            ctx.error(range, err.to_string());
        }

        Some(())
    }
}

impl Eval for ast::Include {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let file_id = ctx.current_file_id();
        let include_map = ctx.db.resolved_include_map(file_id);

        let include_id = IncludeId(SyntaxNodePtr::new(self.syntax()));
        let Some(include_file_id) = include_map.get(&include_id).copied() else {
            let path = self.path().map(|it| it.value()).unwrap_or_default();
            ctx.error(
                self.syntax().text_range(),
                format!("include file not found: {path}"),
            );
            return Some(());
        };

        let parse = ctx.db.parse(include_file_id);
        let source_file = ast::SourceFile::cast(parse.syntax_node())?;

        ctx.push_file(include_file_id);
        source_file.eval(ctx);
        ctx.pop_file();
        Some(())
    }
}

impl Eval for ast::Type {
    type Output = Type;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        match self {
            ast::Type::BitType(_) => Some(Type::Bit),
            ast::Type::IntType(_) => Some(Type::Int),
            ast::Type::StringType(_) => Some(Type::String),
            ast::Type::DagType(_) => Some(Type::Dag),
            ast::Type::BitsType(bits_typ) => Some(Type::Bits(
                bits_typ
                    .length()?
                    .value()?
                    .try_into()
                    .expect("bit length overflow"),
            )),
            ast::Type::ListType(list_typ) => {
                Some(Type::List(Box::new(list_typ.inner_type()?.eval(ctx)?)))
            }
            ast::Type::ClassId(class_id) => {
                let (name, reference_loc) = utils::identifier(class_id.name()?, ctx)?;
                let Some(id) = ctx.symbol_map.find_class(&name) else {
                    ctx.error(
                        class_id.name()?.range()?,
                        format!("class not found: {name}"),
                    );
                    return None;
                };
                ctx.symbol_map.add_reference(id, reference_loc);
                Some(Type::Class(id, name))
            }
            ast::Type::CodeType(_) => Some(Type::Code),
        }
    }
}

impl EvalValue for ast::Value {
    type Output = Expr;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        let inner_values: Vec<_> = self
            .inner_values()
            .filter_map(|it| it.eval_value(ctx, mode))
            .collect();
        if inner_values.is_empty() {
            let loc = FileRange::new(ctx.current_file_id(), self.syntax().text_range());
            return Some(Expr::uninitialized(loc));
        }

        inner_values
            .into_iter()
            .reduce(|lhs, rhs| Expr::Paste(lhs.loc(), Box::new(lhs), Box::new(rhs)))
    }
}

impl EvalValue for ast::InnerValue {
    type Output = Expr;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        let loc = FileRange::new(ctx.current_file_id(), self.syntax().text_range());
        let inner_value = self.simple_value()?.eval_value(ctx, mode)?;
        let mut expr = Expr::Simple(loc, inner_value);
        for suffix in self.suffixes() {
            match suffix {
                ast::ValueSuffix::FieldSuffix(field_suffix) => {
                    expr = eval_field_suffix(ctx, expr, field_suffix)?;
                }
                _ => {
                    ctx.error(
                        suffix.syntax().text_range(),
                        format!("{}:{} not implemented", file!(), line!()),
                    );
                    return None;
                }
            }
        }
        Some(expr)
    }
}

fn eval_field_suffix(
    ctx: &mut EvalCtx,
    expr: Expr,
    field_suffix: ast::FieldSuffix,
) -> Option<Expr> {
    let (field_name, reference_loc) = utils::identifier(field_suffix.name()?, ctx)?;

    // TODO: extract this logic to a function
    let (field_id, field_typ): (SymbolId, _) = match expr.typ() {
        Type::Class(class_id, _) => {
            let class = ctx.symbol_map.class(class_id);
            let Some(field_id) = class.find_field(&field_name) else {
                ctx.error(
                    field_suffix.syntax().text_range(),
                    format!("cannot access field '{field_name}' of value '{expr}'"),
                );
                return None;
            };
            let field = ctx.symbol_map.field(field_id);
            (field_id.into(), field.typ.clone())
        }
        Type::Def(def_id, _) => {
            let def = ctx.symbol_map.def(def_id);
            let Some(field_id) = def.find_field(&field_name) else {
                ctx.error(
                    field_suffix.syntax().text_range(),
                    format!("cannot access field '{field_name}' of value '{expr}'"),
                );
                return None;
            };
            let field = ctx.symbol_map.def_field(field_id);
            (field_id.into(), field.typ.clone())
        }
        _ => {
            ctx.error(
                field_suffix.syntax().text_range(),
                format!("cannot access field '{field_name}' of value '{expr}'"),
            );
            return None;
        }
    };

    ctx.symbol_map.add_reference(field_id, reference_loc);
    Some(Expr::FieldSuffix(
        reference_loc,
        Box::new(expr),
        field_name,
        field_typ,
    ))
}

impl EvalValue for ast::SimpleValue {
    type Output = SimpleExpr;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        let loc = FileRange::new(ctx.current_file_id(), self.syntax().text_range());
        match self {
            ast::SimpleValue::Uninitialized(_) => Some(SimpleExpr::Uninitialized(loc)),
            ast::SimpleValue::Integer(integer) => Some(SimpleExpr::Int(loc, integer.value()?)),
            ast::SimpleValue::String(string) => Some(SimpleExpr::String(loc, string.value())),
            ast::SimpleValue::Code(string) => Some(SimpleExpr::Code(loc, string.value()?)),
            ast::SimpleValue::Boolean(boolean) => Some(SimpleExpr::Boolean(loc, boolean.value()?)),
            ast::SimpleValue::Bits(bits) => {
                let values = bits
                    .value_list()?
                    .values()
                    .filter_map(|it| it.eval_value(ctx, mode))
                    .collect();
                Some(SimpleExpr::Bits(loc, values))
            }
            ast::SimpleValue::List(list) => {
                let values: Vec<_> = list
                    .value_list()?
                    .values()
                    .filter_map(|it| it.eval_value(ctx, mode))
                    .collect();
                let typ = match values.first() {
                    Some(expr) => expr.typ(),
                    None => Type::Unknown,
                };
                Some(SimpleExpr::List(loc, values, typ))
            }
            ast::SimpleValue::Dag(dag) => {
                let op = dag.operator()?.eval_value(ctx, mode)?;
                let args = dag
                    .arg_list()
                    .and_then(|it| it.eval_value(ctx, mode))
                    .unwrap_or_default();
                Some(SimpleExpr::Dag(loc, Box::new(op), args))
            }
            ast::SimpleValue::Identifier(identifier) => match mode {
                EvalValueMode::AsValue => {
                    let (name, reference_loc) = utils::identifier(identifier, ctx)?;
                    let Some(symbol_id) = ctx.resolve_id(&name) else {
                        ctx.error(
                            reference_loc.range,
                            format!("Variable not defined: '{name}'"),
                        );
                        return None;
                    };
                    ctx.symbol_map.add_reference(symbol_id, reference_loc);
                    let symbol = ctx.symbol_map.symbol(symbol_id);
                    let typ = match symbol {
                        Symbol::TemplateArgument(template_arg) => template_arg.typ.clone(),
                        Symbol::Field(field) => field.typ.clone(),
                        Symbol::Def(def) => {
                            let def_id = symbol_id.as_def_id().unwrap();
                            Type::Def(def_id, def.name.clone())
                        }
                        Symbol::Variable(variable) => variable.value.typ().clone(),
                        _ => {
                            ctx.error(
                                reference_loc.range,
                                format!("{}:{} not implemented", file!(), line!()),
                            );
                            Type::Unknown
                        }
                    };
                    Some(SimpleExpr::Identifier(loc, name, symbol_id, typ))
                }
                EvalValueMode::AsIdentifier => {
                    let (name, reference_loc) = utils::identifier(identifier, ctx)?;
                    match ctx.resolve_id_in_current_scope(&name) {
                        None => Some(SimpleExpr::String(loc, name)),
                        Some(variable_id) => {
                            ctx.symbol_map.add_reference(variable_id, reference_loc);
                            let variable = ctx.symbol_map.variable(variable_id);
                            let typ = variable.value.typ().clone();
                            Some(SimpleExpr::Identifier(loc, name, variable_id.into(), typ))
                        }
                    }
                }
            },
            ast::SimpleValue::ClassValue(class_value) => {
                let (name, reference_loc) = utils::identifier(class_value.name()?, ctx)?;
                let Some(class_id) = ctx.symbol_map.find_class(&name) else {
                    ctx.error(reference_loc.range, format!("class not found: {name}"));
                    return None;
                };
                ctx.symbol_map.add_reference(class_id, reference_loc);
                let arg_value_list = class_value
                    .arg_value_list()
                    .and_then(|it| it.eval(ctx))
                    .unwrap_or_default();
                Some(SimpleExpr::ClassValue(loc, name, class_id, arg_value_list))
            }
            ast::SimpleValue::BangOperator(bang_operator) => {
                let op = bang_operator.kind()?.into();
                let args = bang_operator
                    .values()
                    .filter_map(|it| it.eval_value(ctx, mode))
                    .collect();
                Some(SimpleExpr::BangOperator(loc, op, args))
            }
            _ => {
                ctx.error(
                    self.syntax().text_range(),
                    format!("{}:{} not implemented", file!(), line!()),
                );
                None
            }
        }
    }
}

impl EvalValue for ast::DagArg {
    type Output = DagArg;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        Some(DagArg {
            value: self.value()?.eval_value(ctx, mode)?,
            var_name: self.var_name().and_then(|it| it.value()),
        })
    }
}

impl EvalValue for ast::DagArgList {
    type Output = Vec<DagArg>;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        Some(
            self.args()
                .filter_map(|arg| arg.eval_value(ctx, mode))
                .collect(),
        )
    }
}

impl EvalExpr for Expr {
    fn eval_expr(self, ctx: &mut EvalCtx) -> Option<Value> {
        let loc = self.loc();
        match self {
            Expr::Simple(_, simple) => simple.eval_expr(ctx),
            Expr::FieldSuffix(loc, expr, field_name, _) => match expr.eval_expr(ctx)? {
                Value::DefIdentifier(_, def_id, _) => {
                    let def = ctx.symbol_map.def(def_id);
                    let field_id = def.find_field(&field_name).expect("field should exist");
                    let field = ctx.symbol_map.def_field(field_id);
                    Some(field.value.clone())
                }
                value => {
                    ctx.error(
                        loc.range,
                        format!("Cannot access field '{field_name}' of value '{value}'"),
                    );
                    None
                }
            },
            Expr::Paste(_, lhs, rhs) => {
                let lhs = lhs.eval_expr(ctx)?;
                let rhs = rhs.eval_expr(ctx)?;

                // TODO: support conversion from other types
                match (lhs, rhs) {
                    (Value::String(lhs), Value::String(rhs)) => Some(Value::String(lhs + rhs)),
                    _ => {
                        ctx.error(
                            loc.range,
                            format!("{}:{} not implemented", file!(), line!()),
                        );
                        None
                    }
                }
            }
        }
    }
}

impl EvalExpr for SimpleExpr {
    fn eval_expr(self, ctx: &mut EvalCtx) -> Option<Value> {
        match self {
            SimpleExpr::Uninitialized(_) => Some(Value::Uninitialized),
            SimpleExpr::Boolean(_, boolean) => Some(Value::Bit(boolean)),
            SimpleExpr::Int(_, int) => Some(Value::Int(int)),
            SimpleExpr::String(_, string) => Some(Value::String(string)),
            SimpleExpr::Code(_, code) => Some(Value::String(code)),
            SimpleExpr::Bits(_, bits) => {
                let bits_len = bits.len();
                let mut bit_values = Vec::new();
                for (i, bit) in bits.into_iter().enumerate() {
                    let loc = bit.loc();
                    let Some(bit_value) = bit.eval_expr(ctx) else {
                        continue;
                    };

                    match bit_value.cast_to(&ctx.symbol_map, &Type::Bit) {
                        Some(Value::Bit(value)) => bit_values.push(value),
                        _ => {
                            ctx.error(
                                loc.range,
                                format!("Element #{i} ({bit_value}) is not convertable to a bit"),
                            );
                        }
                    }
                }
                if bit_values.len() == bits_len {
                    let value = bit_values
                        .into_iter()
                        .rev()
                        .enumerate()
                        .map(|(i, bit)| (bit as i64) << i)
                        .sum();
                    Some(Value::Bits(value, bits_len))
                } else {
                    None
                }
            }
            SimpleExpr::List(_, values, typ) => {
                let values: Vec<_> = values
                    .into_iter()
                    .filter_map(|it| it.eval_expr(ctx))
                    .collect();
                Some(Value::List(values, typ))
            }
            SimpleExpr::Dag(_, op, args) => {
                let op = DagArgValue {
                    value: op.value.eval_expr(ctx)?,
                    var_name: op.var_name,
                };
                let args = args
                    .into_iter()
                    .filter_map(|it| {
                        Some(DagArgValue {
                            value: it.value.eval_expr(ctx)?,
                            var_name: it.var_name,
                        })
                    })
                    .collect();
                Some(Value::Dag(Box::new(op), args))
            }
            SimpleExpr::Identifier(_, symbol_name, symbol_id, typ) => {
                match ctx.symbol_map.symbol(symbol_id) {
                    Symbol::Class(_) | Symbol::TemplateArgument(_) | Symbol::Field(_) => {
                        tracing::info!(
                            "{symbol_id:?} cannot be used as an identifier. Maybe a bug?"
                        );
                        None
                    }
                    Symbol::Variable(variable) => Some(variable.value.clone()),
                    Symbol::Def(_) => Some(Value::DefIdentifier(
                        symbol_name,
                        symbol_id.as_def_id().unwrap(),
                        typ,
                    )),
                    Symbol::DefField(field) => Some(field.value.clone()),
                }
            }
            SimpleExpr::ClassValue(loc, _, class_id, arg_value_list) => {
                let name = ctx.next_anonymous_def_name();
                let define_loc =
                    FileRange::new(ctx.current_file_id(), TextRange::empty(loc.range.start()));
                let mut record = Record::new(name.clone(), define_loc);

                if let Err((range, err)) =
                    record.inherit(&mut ctx.symbol_map, class_id, arg_value_list, define_loc)
                {
                    ctx.error(range, err.to_string());
                    return None;
                }

                let def = record.into_def(ctx);
                let id = ctx.symbol_map.add_def(def);
                let typ = Type::Def(id, name.clone());
                Some(Value::DefIdentifier(name, id, typ))
            }
            SimpleExpr::BangOperator(loc, op, args) => match op {
                BangOperatorOp::XAdd
                | BangOperatorOp::XSub
                | BangOperatorOp::XMul
                | BangOperatorOp::XDiv
                | BangOperatorOp::XAnd
                | BangOperatorOp::XOr
                | BangOperatorOp::XXor
                | BangOperatorOp::XShl
                | BangOperatorOp::XSra
                | BangOperatorOp::XSrl => {
                    if args.len() < 2 {
                        ctx.error(loc.range, "expected two operands to operator");
                        return None;
                    }

                    let mut acc: i64 = 0;
                    for (i, arg) in args.into_iter().enumerate() {
                        let value = match arg.eval_expr(ctx)? {
                            Value::Int(v) => v,
                            v => {
                                ctx.error(
                                    loc.range,
                                    format!("expected value of type 'int', got '{}'", v.typ()),
                                );
                                return None;
                            }
                        };
                        if i == 0 {
                            acc = value;
                            continue;
                        }

                        acc = match op {
                            BangOperatorOp::XAdd => acc.wrapping_add(value),
                            BangOperatorOp::XSub => acc.wrapping_sub(value),
                            BangOperatorOp::XMul => acc.wrapping_mul(value),
                            BangOperatorOp::XDiv => acc.wrapping_div(value),
                            BangOperatorOp::XAnd => acc & value,
                            BangOperatorOp::XOr => acc | value,
                            BangOperatorOp::XXor => acc ^ value,
                            BangOperatorOp::XShl => acc.wrapping_shl(value as u32),
                            BangOperatorOp::XSra => acc.wrapping_shr(value as u32),
                            BangOperatorOp::XSrl => {
                                ((acc as u64).wrapping_shr(value as u32)) as i64
                            }
                            _ => unreachable!(),
                        };
                    }

                    Some(Value::Int(acc))
                }
                BangOperatorOp::XEq
                | BangOperatorOp::XNe
                | BangOperatorOp::XLe
                | BangOperatorOp::XLt
                | BangOperatorOp::XGe
                | BangOperatorOp::XGt => {
                    if args.len() != 2 {
                        ctx.error(loc.range, "expected two operands to operator");
                        return None;
                    }

                    let mut iter_args = args.into_iter();
                    let lhs = iter_args.next().unwrap().eval_expr(ctx)?;
                    let rhs = iter_args.next().unwrap().eval_expr(ctx)?;

                    match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => match op {
                            BangOperatorOp::XEq => Some(Value::Bit(lhs == rhs)),
                            BangOperatorOp::XNe => Some(Value::Bit(lhs != rhs)),
                            BangOperatorOp::XLe => Some(Value::Bit(lhs < rhs)),
                            BangOperatorOp::XLt => Some(Value::Bit(lhs < rhs)),
                            BangOperatorOp::XGe => Some(Value::Bit(lhs >= rhs)),
                            BangOperatorOp::XGt => Some(Value::Bit(lhs > rhs)),
                            _ => unreachable!(),
                        },
                        (Value::String(lhs), Value::String(rhs)) => match op {
                            BangOperatorOp::XEq => Some(Value::Bit(lhs == rhs)),
                            BangOperatorOp::XNe => Some(Value::Bit(lhs != rhs)),
                            BangOperatorOp::XLe => Some(Value::Bit(lhs < rhs)),
                            BangOperatorOp::XLt => Some(Value::Bit(lhs < rhs)),
                            BangOperatorOp::XGe => Some(Value::Bit(lhs >= rhs)),
                            BangOperatorOp::XGt => Some(Value::Bit(lhs > rhs)),
                            _ => unreachable!(),
                        },
                        _ => None,
                    }
                }
                BangOperatorOp::XSize => {
                    if args.len() != 1 {
                        ctx.error(loc.range, "expected one operand in unary operator");
                        return None;
                    }

                    let arg = args.into_iter().next().unwrap();
                    let arg_loc = arg.loc();
                    match arg.eval_expr(ctx)? {
                        Value::String(value) => Some(Value::Int(value.len() as i64)),
                        Value::List(values, _) => Some(Value::Int(values.len() as i64)),
                        Value::Dag(_, args) => Some(Value::Int(args.len() as i64)),
                        _ => {
                            ctx.error(
                                arg_loc.range,
                                "expected string, list, or dag type argument in unary operator",
                            );
                            None
                        }
                    }
                }
                BangOperatorOp::XSubstr => {
                    if !matches!(args.len(), 2 | 3) {
                        ctx.error(loc.range, "expected two or three operand to operator");
                        return None;
                    }
                    let mut iter_args = args.into_iter();

                    let arg1 = iter_args.next().unwrap();
                    let arg1_loc = arg1.loc();
                    let arg1_value = arg1.eval_expr(ctx)?;
                    let Value::String(string) = arg1_value else {
                        ctx.error(
                            arg1_loc.range,
                            format!("expected string, got type '{}'", arg1_value.typ()),
                        );
                        return None;
                    };

                    let arg2 = iter_args.next().unwrap();
                    let arg2_loc = arg2.loc();
                    let arg2_value = arg2.eval_expr(ctx)?;
                    let Value::Int(start) = arg2_value else {
                        ctx.error(
                            arg2_loc.range,
                            format!("expected int, got type '{}'", arg2_value.typ()),
                        );
                        return None;
                    };

                    let end = match iter_args.next() {
                        Some(arg3) => {
                            let arg3_loc = arg3.loc();
                            let arg3_value = arg3.eval_expr(ctx)?;
                            let Value::Int(end) = arg3_value else {
                                ctx.error(
                                    arg3_loc.range,
                                    format!("expected int, got type '{}'", arg3_value.typ()),
                                );
                                return None;
                            };
                            end
                        }
                        None => string.len() as i64,
                    };

                    string
                        .get(start as usize..end as usize)
                        .map(|s| Value::String(s.into()))
                }
                BangOperatorOp::XStrConcat => {
                    let mut result = String::new();
                    for arg in args {
                        let arg_loc = arg.loc();
                        let arg_value = arg.eval_expr(ctx)?;
                        let Value::String(value) = arg_value else {
                            ctx.error(
                                arg_loc.range,
                                format!("expected string, got type '{}'", arg_value.typ()),
                            );
                            return None;
                        };
                        result.push_str(&value);
                    }
                    Some(Value::String(result.into()))
                }
                BangOperatorOp::XNot => {
                    if args.len() != 1 {
                        ctx.error(loc.range, "expected one operand in unary operator");
                        return None;
                    }

                    let arg = args.into_iter().next().unwrap();
                    match arg.eval_expr(ctx)? {
                        Value::Bit(value) => Some(Value::Bit(!value)),
                        _ => Some(Value::Bit(false)),
                    }
                }
                _ => {
                    ctx.error(
                        loc.range,
                        format!("{}:{} not implemented", file!(), line!()),
                    );
                    None
                }
            },
            SimpleExpr::CondOperator(loc, _) => {
                ctx.error(
                    loc.range,
                    format!("{}:{} not implemented", file!(), line!()),
                );
                None
            }
        }
    }
}

mod utils {
    use ecow::EcoString;

    use syntax::ast;

    use crate::file_system::FileRange;

    use super::EvalCtx;

    pub(super) fn identifier(
        identifier: ast::Identifier,
        ctx: &mut EvalCtx,
    ) -> Option<(EcoString, FileRange)> {
        let name = identifier.value()?;
        let loc = FileRange::new(ctx.current_file_id(), identifier.range()?);
        Some((name, loc))
    }
}
