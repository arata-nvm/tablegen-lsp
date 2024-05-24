use std::sync::Arc;

use ecow::{eco_format, EcoString};

use syntax::ast::AstNode;
use syntax::ast::{self};
use syntax::parser::TextRange;
use syntax::SyntaxNodePtr;

use crate::db::SourceDatabase;
use crate::file_system::{FileId, FileRange, IncludeId};
use crate::handlers::diagnostics::Diagnostic;
use crate::symbol_map::{
    Class, ClassId, Def, Expr, Field, SimpleExpr, Symbol, SymbolId, SymbolMap, TemplateArgument,
    Type, Value,
};

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

pub struct EvalCtx<'a> {
    db: &'a dyn EvalDatabase,
    file_trace: Vec<FileId>,
    symbol_map: SymbolMap,
    diagnostics: Vec<Diagnostic>,
    scopes: Scopes,
}

impl<'a> EvalCtx<'a> {
    pub fn new(db: &'a dyn EvalDatabase, root_file: FileId) -> Self {
        Self {
            db,
            file_trace: vec![root_file],
            symbol_map: SymbolMap::default(),
            diagnostics: Vec::new(),
            scopes: Scopes::default(),
        }
    }

    pub fn current_file_id(&self) -> FileId {
        *self.file_trace.last().expect("file_trace is empty")
    }

    pub fn push_file(&mut self, file_id: FileId) {
        self.file_trace.push(file_id);
    }

    pub fn pop_file(&mut self) {
        self.file_trace.pop().expect("file_trace is empty");
    }

    pub fn error(&mut self, range: TextRange, message: impl Into<String>) {
        let file = self.current_file_id();
        self.diagnostics
            .push(Diagnostic::new(FileRange::new(file, range), message));
    }

    pub fn finish(self) -> Evaluation {
        Evaluation {
            symbol_map: self.symbol_map,
            diagnostics: self.diagnostics,
        }
    }
}

#[derive(Debug, Default)]
struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    fn pop(&mut self) -> Scope {
        self.scopes.pop().expect("scope is empty")
    }

    fn current_class_id(&self) -> ClassId {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::Class(id, _) => Some(*id),
                _ => None,
            })
            .expect("scope is empty")
    }

    fn current_class(&self) -> &Class {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::Class(_, class) => Some(class),
                _ => None,
            })
            .expect("scope is empty")
    }

    fn current_class_mut(&mut self) -> &mut Class {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| match scope {
                Scope::Class(_, class) => Some(class),
                _ => None,
            })
            .expect("scope is empty")
    }

    fn find_var(&self, name: &EcoString) -> Option<SymbolId> {
        // TODO
        if self.scopes.is_empty() {
            return None;
        }

        let class = self.current_class();

        if let Some(field_id) = class.find_field(name) {
            return Some(field_id.into());
        }

        if let Some(template_arg_id) = class.find_template_arg(name) {
            return Some(template_arg_id.into());
        }

        None
    }
}

#[derive(Debug)]
enum Scope {
    Class(ClassId, Class),
}

impl Scope {
    pub fn into_class(self) -> (ClassId, Class) {
        match self {
            Scope::Class(id, class) => (id, class),
            _ => panic!("not a class"),
        }
    }
}

pub trait Eval {
    type Output;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output>;
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
            _ => {
                ctx.error(self.syntax().text_range(), "not implemented");
                Some(())
            }
        }
    }
}

impl Eval for ast::Class {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let class = Class::new(name.clone(), define_loc);
        let id = ctx.symbol_map.add_class(class.clone());

        ctx.scopes.push(Scope::Class(id, class));
        if let Some(list) = self.template_arg_list() {
            list.eval(ctx);
        }
        if let Some(body) = self.record_body() {
            body.eval(ctx);
        }
        let (_, real_class) = ctx.scopes.pop().into_class();
        ctx.symbol_map.replace_class(id, real_class);

        Some(())
    }
}

impl Eval for ast::Def {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let name_range = self.name()?.syntax().text_range();
        let define_loc = FileRange::new(ctx.current_file_id(), name_range);
        let name = self.name()?.eval(ctx)?.eval_identifier(ctx, define_loc)?;
        let def = Def::new(name.clone(), define_loc);
        let _ = ctx.symbol_map.add_def(def);
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
            .current_class_mut()
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

        if let Err((range, err)) = ctx.scopes.current_class_mut().inherit(
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
            ctx.error(self.syntax().text_range(), "not implemented");
        }
        positional_arg_value_list
    }
}

impl Eval for ast::PositionalArgValueList {
    type Output = Vec<Expr>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        Some(self.values().filter_map(|it| it.eval(ctx)).collect())
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
                ctx.error(self.syntax().text_range(), "not implemented");
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
            .and_then(|it| it.eval(ctx))
            .unwrap_or(Expr::Simple(SimpleExpr::Uninitialized));
        let field = Field::new(
            name.clone(),
            typ,
            value,
            ctx.scopes.current_class_id(),
            define_loc,
        );

        if let Err((range, err)) = ctx
            .scopes
            .current_class_mut()
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
            .and_then(|it| it.eval(ctx))
            .unwrap_or(Expr::Simple(SimpleExpr::Uninitialized));

        let Some(old_field_id) = ctx.scopes.current_class().find_field(&name) else {
            ctx.error(define_loc.range, format!("unknown field: {name}"));
            return None;
        };
        let old_field = ctx.symbol_map.field(old_field_id);

        let new_field = old_field.modified(value, ctx.scopes.current_class_id(), define_loc);
        if let Err((range, err)) = ctx
            .scopes
            .current_class_mut()
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

impl Eval for ast::Value {
    type Output = Expr;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let inner_values: Vec<_> = self.inner_values().filter_map(|it| it.eval(ctx)).collect();
        if inner_values.len() != 1 {
            ctx.error(self.syntax().text_range(), "not implemented");
            return None;
        }
        Some(inner_values.into_iter().next().unwrap())
    }
}

impl Eval for ast::InnerValue {
    type Output = Expr;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let mut expr = Expr::Simple(self.simple_value()?.eval(ctx)?);
        for suffix in self.suffixes() {
            match suffix {
                ast::ValueSuffix::FieldSuffix(field_suffix) => {
                    expr = eval_field_suffix(ctx, expr, field_suffix)?;
                }
                _ => {
                    ctx.error(suffix.syntax().text_range(), "not implemented");
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

impl Eval for ast::SimpleValue {
    type Output = SimpleExpr;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        match self {
            ast::SimpleValue::Uninitialized(_) => Some(SimpleExpr::Uninitialized),
            ast::SimpleValue::Integer(integer) => integer.value().map(SimpleExpr::Int),
            ast::SimpleValue::String(string) => Some(SimpleExpr::String(string.value())),
            ast::SimpleValue::Code(string) => string.value().map(SimpleExpr::Code),
            ast::SimpleValue::Boolean(boolean) => boolean.value().map(SimpleExpr::Boolean),
            ast::SimpleValue::Bits(bits) => bits
                .value_list()
                .map(|list| list.values().filter_map(|it| it.eval(ctx)).collect())
                .map(SimpleExpr::Bits),
            ast::SimpleValue::List(list) => {
                let values: Vec<_> = list
                    .value_list()?
                    .values()
                    .filter_map(|it| it.eval(ctx))
                    .collect();
                let typ = match values.first() {
                    Some(expr) => expr.typ(),
                    None => Type::Unknown,
                };
                Some(SimpleExpr::List(values, typ))
            }
            ast::SimpleValue::Identifier(identifier) => {
                let (name, reference_loc) = utils::identifier(identifier, ctx)?;
                let symbol_sig = match ctx.scopes.find_var(&name) {
                    Some(symbol_id) => {
                        ctx.symbol_map.add_reference(symbol_id, reference_loc);
                        let symbol = ctx.symbol_map.symbol(symbol_id);
                        let typ = match symbol {
                            Symbol::TemplateArgument(template_arg) => template_arg.typ.clone(),
                            Symbol::Field(field) => field.typ.clone(),
                            _ => {
                                ctx.error(reference_loc.range, "not implemented");
                                Type::Unknown
                            }
                        };
                        Some((symbol_id, typ))
                    }
                    None => None,
                };
                Some(SimpleExpr::Identifier(name, symbol_sig))
            }
            ast::SimpleValue::BangOperator(bang_operator) => {
                let op = bang_operator.kind()?.into();
                let args = bang_operator
                    .values()
                    .filter_map(|it| it.eval(ctx))
                    .collect();
                Some(SimpleExpr::BangOperator(op, args))
            }
            _ => {
                ctx.error(self.syntax().text_range(), "not implemented");
                None
            }
        }
    }
}

pub trait ValueEval {
    fn eval_identifier(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<EcoString>;
    fn eval_value(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<Value>;
}

impl ValueEval for Expr {
    fn eval_identifier(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<EcoString> {
        match self {
            Expr::Simple(simple) => simple.eval_identifier(ctx, loc),
            _ => {
                ctx.error(loc.range, "not implemented");
                None
            }
        }
    }

    fn eval_value(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<Value> {
        match self {
            Expr::Simple(simple) => simple.eval_value(ctx, loc),
            _ => {
                ctx.error(loc.range, "not implemented");
                None
            }
        }
    }
}

impl ValueEval for SimpleExpr {
    fn eval_identifier(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<EcoString> {
        match self {
            SimpleExpr::Uninitialized | SimpleExpr::Bits(_) | SimpleExpr::List(_, _) => {
                ctx.error(loc.range, "'{self}' cannot be used as an identifier");
                None
            }
            SimpleExpr::Boolean(false) => Some("0".into()),
            SimpleExpr::Boolean(true) => Some("1".into()),
            SimpleExpr::Int(int) => Some(eco_format!("{int}")),
            SimpleExpr::String(string) => Some(string.into()),
            SimpleExpr::Code(code) => Some(code),
            SimpleExpr::Identifier(name, _) => Some(name),
            SimpleExpr::BangOperator(_, _) => {
                ctx.error(loc.range, "not implemented");
                None
            }
        }
    }

    fn eval_value(self, ctx: &mut EvalCtx, loc: FileRange) -> Option<Value> {
        match self {
            SimpleExpr::Uninitialized => Some(Value::Uninitialized),
            SimpleExpr::Boolean(boolean) => Some(Value::Int(boolean as i64)),
            SimpleExpr::Int(int) => Some(Value::Int(int)),
            SimpleExpr::String(string) => Some(Value::String(string)),
            SimpleExpr::Code(code) => Some(Value::String(code.to_string())),
            SimpleExpr::Bits(bits) => {
                let bits: Vec<_> = bits
                    .into_iter()
                    .filter_map(|it| it.eval_value(ctx, loc))
                    .collect();
                Some(Value::Bits(bits))
            }
            SimpleExpr::List(values, typ) => {
                let values: Vec<_> = values
                    .into_iter()
                    .filter_map(|it| it.eval_value(ctx, loc))
                    .collect();
                Some(Value::List(values, typ))
            }
            SimpleExpr::Identifier(_, Some(_)) => {
                ctx.error(loc.range, "not implemented");
                None
            }
            SimpleExpr::Identifier(name, None) => {
                ctx.error(loc.range, format!("symbol not found: {name}"));
                None
            }
            SimpleExpr::BangOperator(_, _) => {
                ctx.error(loc.range, "not implemented");
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
