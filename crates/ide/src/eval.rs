use std::sync::Arc;

use syntax::ast::AstNode;
use syntax::ast::{self};
use syntax::SyntaxNodePtr;

use crate::db::SourceDatabase;
use crate::file_system::{FileRange, IncludeId};
use crate::handlers::diagnostics::Diagnostic;
use crate::symbol_map::expr::{DagArg, Expr, SimpleExpr};
use crate::symbol_map::field::Field;
use crate::symbol_map::record::Record;
use crate::symbol_map::symbol::Symbol;
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
    fn eval_expr(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<Value>;
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
        let name_range = self.name()?.syntax().text_range();
        let define_loc = FileRange::new(ctx.current_file_id(), name_range);
        let name = self
            .name()?
            .eval_value(ctx, EvalValueMode::AsIdentifier)?
            .eval_expr(ctx, define_loc)?;
        let Value::String(name) = name else {
            ctx.error(
                define_loc.range,
                format!("'{name}' cannot be used as an identifier"),
            );
            return None;
        };
        let record = Record::new(name.clone(), define_loc);
        let id = ctx.symbol_map.add_def(record.clone());

        ctx.scopes.push(ScopeKind::Def(id, record));
        if let Some(body) = self.record_body() {
            body.eval(ctx);
        }
        let (_, record) = ctx.scopes.pop().into_def();
        ctx.symbol_map.replace_def(id, record);

        Some(())
    }
}

impl Eval for ast::Defvar {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let value = self.value()?.eval_value(ctx, EvalValueMode::AsValue)?;
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
        let template_arg = TemplateArgument::new(name.clone(), typ, define_loc);
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
            .unwrap_or(Expr::Simple(SimpleExpr::Uninitialized));
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
            .unwrap_or(Expr::Simple(SimpleExpr::Uninitialized));

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
            return Some(Expr::Simple(SimpleExpr::Uninitialized));
        }

        if inner_values.len() != 1 {
            ctx.error(
                self.syntax().text_range(),
                format!("{}:{} not implemented", file!(), line!()),
            );
            return None;
        }

        Some(inner_values.into_iter().next().unwrap())
    }
}

impl EvalValue for ast::InnerValue {
    type Output = Expr;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        let mut expr = Expr::Simple(self.simple_value()?.eval_value(ctx, mode)?);
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
    let Type::Class(class_id, _) = expr.typ() else {
        ctx.error(
            field_suffix.syntax().text_range(),
            format!("cannot access field '{field_name}' of value '{expr}'"),
        );
        return None;
    };

    let class = ctx.symbol_map.class(class_id);
    let Some(field_id) = class.find_field(&field_name) else {
        ctx.error(
            field_suffix.syntax().text_range(),
            format!("cannot access field '{field_name}' of value '{expr}'"),
        );
        return None;
    };
    ctx.symbol_map.add_reference(field_id, reference_loc);

    let field = ctx.symbol_map.field(field_id);
    Some(Expr::FieldSuffix(
        Box::new(expr),
        field_name,
        field.typ.clone(),
    ))
}

impl EvalValue for ast::SimpleValue {
    type Output = SimpleExpr;
    fn eval_value(self, ctx: &mut EvalCtx, mode: EvalValueMode) -> Option<Self::Output> {
        match self {
            ast::SimpleValue::Uninitialized(_) => Some(SimpleExpr::Uninitialized),
            ast::SimpleValue::Integer(integer) => integer.value().map(SimpleExpr::Int),
            ast::SimpleValue::String(string) => Some(SimpleExpr::String(string.value())),
            ast::SimpleValue::Code(string) => string.value().map(SimpleExpr::Code),
            ast::SimpleValue::Boolean(boolean) => boolean.value().map(SimpleExpr::Boolean),
            ast::SimpleValue::Bits(bits) => bits
                .value_list()
                .map(|list| {
                    list.values()
                        .filter_map(|it| it.eval_value(ctx, mode))
                        .collect()
                })
                .map(SimpleExpr::Bits),
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
                Some(SimpleExpr::List(values, typ))
            }
            ast::SimpleValue::Dag(dag) => {
                let op = dag.operator()?.eval_value(ctx, mode)?;
                let args = dag
                    .arg_list()
                    .and_then(|it| it.eval_value(ctx, mode))
                    .unwrap_or_default();
                Some(SimpleExpr::Dag(Box::new(op), args))
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
                            Type::Def(symbol_id.as_def_id().unwrap(), def.name.clone())
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
                    Some(SimpleExpr::Identifier(name, symbol_id, typ))
                }
                EvalValueMode::AsIdentifier => {
                    let (name, reference_loc) = utils::identifier(identifier, ctx)?;
                    match ctx.resolve_id_in_current_scope(&name) {
                        None => Some(SimpleExpr::String(name)),
                        Some(variable_id) => {
                            ctx.symbol_map.add_reference(variable_id, reference_loc);
                            let variable = ctx.symbol_map.variable(variable_id);
                            let typ = variable.value.typ().clone();
                            Some(SimpleExpr::Identifier(name, variable_id.into(), typ))
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
                let arg_value_list = class_value
                    .arg_value_list()
                    .and_then(|it| it.eval(ctx))
                    .unwrap_or_default();
                Some(SimpleExpr::ClassValue(name, class_id, arg_value_list))
            }
            ast::SimpleValue::BangOperator(bang_operator) => {
                let op = bang_operator.kind()?.into();
                let args = bang_operator
                    .values()
                    .filter_map(|it| it.eval_value(ctx, mode))
                    .collect();
                Some(SimpleExpr::BangOperator(op, args))
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
    fn eval_expr(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<Value> {
        match self {
            Expr::Simple(simple) => simple.eval_expr(ctx, loc),
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

impl EvalExpr for SimpleExpr {
    fn eval_expr(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<Value> {
        match self {
            SimpleExpr::Uninitialized => Some(Value::Uninitialized),
            SimpleExpr::Boolean(boolean) => Some(Value::Int(boolean as i64)),
            SimpleExpr::Int(int) => Some(Value::Int(int)),
            SimpleExpr::String(string) => Some(Value::String(string)),
            SimpleExpr::Code(code) => Some(Value::String(code)),
            SimpleExpr::Bits(bits) => {
                let bits: Vec<_> = bits
                    .into_iter()
                    .filter_map(|it| it.eval_expr(ctx, loc))
                    .collect();
                Some(Value::Bits(bits))
            }
            SimpleExpr::List(values, typ) => {
                let values: Vec<_> = values
                    .into_iter()
                    .filter_map(|it| it.eval_expr(ctx, loc))
                    .collect();
                Some(Value::List(values, typ))
            }
            SimpleExpr::Dag(op, args) => {
                let op = DagArgValue {
                    value: op.value.eval_expr(ctx, loc)?,
                    var_name: op.var_name,
                };
                let args = args
                    .into_iter()
                    .filter_map(|it| {
                        Some(DagArgValue {
                            value: it.value.eval_expr(ctx, loc)?,
                            var_name: it.var_name,
                        })
                    })
                    .collect();
                Some(Value::Dag(Box::new(op), args))
            }
            SimpleExpr::Identifier(_, _, _) => {
                ctx.error(
                    loc.range,
                    format!("{}:{} not implemented", file!(), line!()),
                );
                None
            }
            SimpleExpr::ClassValue(_, _, _) => {
                ctx.error(
                    loc.range,
                    format!("{}:{} not implemented", file!(), line!()),
                );
                None
            }
            SimpleExpr::BangOperator(_, _) => {
                ctx.error(
                    loc.range,
                    format!("{}:{} not implemented", file!(), line!()),
                );
                None
            }
            SimpleExpr::CondOperator(_) => {
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
