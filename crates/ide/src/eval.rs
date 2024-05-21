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
    Class, ClassId, Field, FieldId, SymbolId, SymbolMap, SymbolMapBuilder, TemplateArgument,
    TemplateArgumentId,
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
        let name = self.name()?.eval(ctx)?;
        let define_loc = FileRange::new(ctx.current_file_id(), self.name()?.syntax().text_range());

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

        let class = Class::new(
            name.clone(),
            define_loc,
            template_arg_list,
            parent_class_list,
            field_list,
        );
        let id = ctx.symbol_map.add_class(class);
        ctx.scope.add_symbol(name, id);

        ctx.scope.pop();
        Some(())
    }
}

impl Eval for ast::TemplateArgList {
    type Output = Vec<TemplateArgumentId>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        Some(self.args().filter_map(|it| it.eval(ctx)).collect())
    }
}

impl Eval for ast::TemplateArgDecl {
    type Output = TemplateArgumentId;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let name = self.name()?.eval(ctx)?;
        let define_loc = FileRange::new(ctx.current_file_id(), self.name()?.syntax().text_range());
        let template_arg = TemplateArgument::new(name.clone(), define_loc);
        let id = ctx.symbol_map.add_template_argument(template_arg);
        ctx.scope.add_symbol(name, id);
        Some(id)
    }
}

impl Eval for ast::RecordBody {
    type Output = (Vec<ClassId>, Vec<FieldId>);
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
        let name = self.name()?.value()?;
        let Some(class_id) = ctx.scope.find_symbol(&name).and_then(|it| it.as_class_id()) else {
            ctx.error(
                self.syntax().text_range(),
                format!("class not found: {name}"),
            );
            return None;
        };
        let range = FileRange::new(ctx.current_file_id(), self.name()?.syntax().text_range());
        ctx.symbol_map.add_reference(class_id, range);
        Some(class_id)
    }
}

impl Eval for ast::Body {
    type Output = Vec<FieldId>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let field_list = self.items().filter_map(|it| it.eval(ctx)).collect();
        Some(field_list)
    }
}

impl Eval for ast::BodyItem {
    type Output = FieldId;
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
    type Output = FieldId;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let name = self.name()?.eval(ctx)?;
        let define_loc = FileRange::new(ctx.current_file_id(), self.name()?.syntax().text_range());
        let _ = self.value()?.eval(ctx);
        let field = Field::new(name.clone(), define_loc);
        let id = ctx.symbol_map.add_field(field);
        ctx.scope.add_symbol(name, id);
        Some(id)
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

impl Eval for ast::Value {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let inner_values: Vec<_> = self
            .inner_values()
            .filter_map(|it| it.simple_value())
            .filter_map(|it| it.eval(ctx))
            .collect();
        if inner_values.len() != 1 {
            ctx.error(self.syntax().text_range(), "not implemented");
        }
        Some(())
    }
}

impl Eval for ast::SimpleValue {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let range = self.syntax().text_range();
        match self {
            ast::SimpleValue::Identifier(identifier) => {
                let name = identifier.eval(ctx)?;
                let Some(symbol_id) = ctx.scope.find_symbol(&name) else {
                    ctx.error(range, format!("symbol not found: {name}"));
                    return None;
                };
                let reference_loc = FileRange::new(ctx.current_file_id(), range);
                ctx.symbol_map.add_reference(symbol_id, reference_loc);
                Some(())
            }
            _ => {
                ctx.error(self.syntax().text_range(), "not implemented");
                None
            }
        }
    }
}

impl Eval for ast::Identifier {
    type Output = EcoString;
    fn eval(self, _ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.value()
    }
}
