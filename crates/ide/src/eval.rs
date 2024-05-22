use std::collections::HashMap;
use std::sync::Arc;

use ecow::EcoString;

use syntax::ast::AstNode;
use syntax::ast::{self};
use syntax::parser::TextRange;
use syntax::SyntaxNodePtr;

use crate::db::SourceDatabase;
use crate::file_system::{FileId, FileRange, IncludeId};
use crate::handlers::diagnostics::Diagnostic;
use crate::symbol_map::{
    Class, ClassId, Expr, Field, FieldId, SimpleExpr, SymbolId, SymbolMap, SymbolMapBuilder,
    TemplateArgument, TemplateArgumentId, Type,
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
    symbol_map: SymbolMapBuilder,
    diagnostics: Vec<Diagnostic>,
    scope: Scope,
}

impl<'a> EvalCtx<'a> {
    pub fn new(db: &'a dyn EvalDatabase, root_file: FileId) -> Self {
        Self {
            db,
            file_trace: vec![root_file],
            symbol_map: SymbolMapBuilder::default(),
            diagnostics: Vec::new(),
            scope: Scope::new(),
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
            symbol_map: self.symbol_map.build(),
            diagnostics: self.diagnostics,
        }
    }
}

#[derive(Debug)]
struct Scope {
    scopes: Vec<HashMap<EcoString, SymbolId>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.scopes.pop().expect("scope is empty");
    }

    fn add_symbol(&mut self, name: EcoString, symbol_id: impl Into<SymbolId>) {
        self.scopes
            .last_mut()
            .expect("scope is empty")
            .insert(name, symbol_id.into());
    }

    fn find_symbol(&mut self, name: &EcoString) -> Option<SymbolId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|it| it.get(name))
            .cloned()
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
        let dummy_class = Class::new(name.clone(), define_loc);
        let id = ctx.symbol_map.add_class(dummy_class);
        ctx.scope.add_symbol(name.clone(), id);

        ctx.scope.push();
        let template_arg_list = self
            .template_arg_list()
            .and_then(|it| it.eval(ctx))
            .unwrap_or_default();

        let (parent_class_list, field_list) = self
            .record_body()
            .and_then(|it| it.eval(ctx))
            .unwrap_or_default();
        ctx.scope.pop();

        let mut real_class = Class::new(name, define_loc);
        for (name, template_arg_id) in template_arg_list {
            real_class.add_template_arg(name, template_arg_id);
        }
        for parent_class_id in parent_class_list {
            real_class.inherit(&ctx.symbol_map.0, parent_class_id);
        }
        for (name, field_id) in field_list {
            if let Err((range, err)) = real_class.add_field(&ctx.symbol_map.0, name, field_id) {
                ctx.error(range, err.to_string());
            }
        }

        let class_ref = ctx.symbol_map.0.class_mut(id);
        let _ = std::mem::replace(class_ref, real_class);

        Some(())
    }
}

impl Eval for ast::TemplateArgList {
    type Output = Vec<(EcoString, TemplateArgumentId)>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        Some(self.args().filter_map(|it| it.eval(ctx)).collect())
    }
}

impl Eval for ast::TemplateArgDecl {
    type Output = (EcoString, TemplateArgumentId);
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let typ = self.r#type()?.eval(ctx)?;
        let template_arg = TemplateArgument::new(name.clone(), typ, define_loc);
        let id = ctx.symbol_map.add_template_argument(template_arg);
        ctx.scope.add_symbol(name.clone(), id);
        Some((name, id))
    }
}

impl Eval for ast::RecordBody {
    type Output = (Vec<ClassId>, Vec<(EcoString, FieldId)>);
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let parent_class_list = self.parent_class_list().and_then(|it| it.eval(ctx));
        let field_list = self.body().and_then(|it| it.eval(ctx));
        parent_class_list.zip(field_list)
    }
}

impl Eval for ast::ParentClassList {
    type Output = Vec<ClassId>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        Some(self.classes().filter_map(|node| node.eval(ctx)).collect())
    }
}

impl Eval for ast::ClassRef {
    type Output = ClassId;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, reference_loc) = utils::identifier(self.name()?, ctx)?;
        let Some(class_id) = ctx.scope.find_symbol(&name).and_then(|it| it.as_class_id()) else {
            ctx.error(reference_loc.range, format!("class not found: {name}"));
            return None;
        };
        ctx.symbol_map.add_reference(class_id, reference_loc);
        self.arg_value_list().and_then(|it| it.eval(ctx));
        Some(class_id)
    }
}

impl Eval for ast::ArgValueList {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.positional().and_then(|it| it.eval(ctx));
        Some(())
    }
}

impl Eval for ast::PositionalArgValueList {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let _: Vec<_> = self.values().filter_map(|it| it.eval(ctx)).collect();
        Some(())
    }
}

impl Eval for ast::Body {
    type Output = Vec<(EcoString, FieldId)>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let field_list = self.items().filter_map(|it| it.eval(ctx)).collect();
        Some(field_list)
    }
}

impl Eval for ast::BodyItem {
    type Output = (EcoString, FieldId);
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        match self {
            ast::BodyItem::FieldDef(field_def) => field_def.eval(ctx),
            _ => {
                ctx.error(self.syntax().text_range(), "not implemented");
                None
            }
        }
    }
}

impl Eval for ast::FieldDef {
    type Output = (EcoString, FieldId);
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(self.name()?, ctx)?;
        let typ = self.r#type()?.eval(ctx)?;
        let value = self
            .value()
            .and_then(|it| it.eval(ctx))
            .unwrap_or(Expr::Simple(SimpleExpr::Uninitialized));
        let field = Field::new(name.clone(), typ, value, define_loc);
        let id = ctx.symbol_map.add_field(field);
        ctx.scope.add_symbol(name.clone(), id);
        Some((name, id))
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
            ast::Type::BitsType(bits_typ) => Some(Type::Bits(bits_typ.length()?.value()?)),
            ast::Type::ListType(list_typ) => {
                Some(Type::List(Box::new(list_typ.inner_type()?.eval(ctx)?)))
            }
            ast::Type::ClassId(class_id) => {
                let (name, reference_loc) = utils::identifier(class_id.name()?, ctx)?;
                let Some(id) = ctx.scope.find_symbol(&name).and_then(|it| it.as_class_id()) else {
                    ctx.error(
                        class_id.name()?.range()?,
                        format!("class not found: {name}"),
                    );
                    return None;
                };
                ctx.symbol_map.add_reference(id, reference_loc);
                Some(Type::Class((id, name)))
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
        Some(Expr::Simple(self.simple_value()?.eval(ctx)?))
    }
}

impl Eval for ast::SimpleValue {
    type Output = SimpleExpr;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        match self {
            ast::SimpleValue::Uninitialized(_) => Some(SimpleExpr::Uninitialized),
            ast::SimpleValue::Integer(integer) => integer.value().map(SimpleExpr::Integer),
            ast::SimpleValue::String(string) => Some(SimpleExpr::String(string.value())),
            ast::SimpleValue::Code(string) => string.value().map(SimpleExpr::Code),
            ast::SimpleValue::Boolean(boolean) => boolean.value().map(SimpleExpr::Boolean),
            ast::SimpleValue::Bits(bits) => bits
                .value_list()
                .map(|list| list.values().filter_map(|it| it.eval(ctx)).collect())
                .map(SimpleExpr::Bits),
            ast::SimpleValue::List(list) => list
                .value_list()
                .map(|list| list.values().filter_map(|it| it.eval(ctx)).collect())
                .map(SimpleExpr::List),
            ast::SimpleValue::Identifier(identifier) => {
                let (name, reference_loc) = utils::identifier(identifier, ctx)?;
                let Some(symbol_id) = ctx.scope.find_symbol(&name) else {
                    ctx.error(reference_loc.range, format!("symbol not found: {name}"));
                    return None;
                };
                ctx.symbol_map.add_reference(symbol_id, reference_loc);
                Some(SimpleExpr::Identifier((symbol_id, name)))
            }
            _ => {
                ctx.error(self.syntax().text_range(), "not implemented");
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
