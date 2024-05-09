use std::collections::HashMap;
use std::sync::Arc;

use ecow::{eco_format, EcoString};

use syntax::ast::AstNode;
use syntax::ast::{self};
use syntax::parser::TextRange;
use syntax::SyntaxNodePtr;

use crate::db::SourceDatabase;
use crate::file_system::{FileId, FileRange, IncludeId};
use crate::handlers::diagnostics::Diagnostic;
use crate::symbol_map::{Class, ClassId, SymbolMap};

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
    scope: Scope,
}

impl<'a> EvalCtx<'a> {
    pub fn new(db: &'a dyn EvalDatabase, root_file: FileId) -> Self {
        Self {
            db,
            file_trace: vec![root_file],
            symbol_map: SymbolMap::default(),
            diagnostics: Vec::new(),
            scope: Scope::default(),
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
struct Scope {
    scope: HashMap<EcoString, ClassId>,
}

impl Scope {
    fn add_class(&mut self, name: EcoString, class_id: ClassId) {
        self.scope.insert(name, class_id);
    }

    fn find_class(&mut self, name: &EcoString) -> Option<ClassId> {
        self.scope.get(name).cloned()
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
        let cur_file = ctx.current_file_id();
        let name_node = self.name()?;
        let define_loc = FileRange::new(cur_file, name_node.syntax().text_range());
        let class = Class::new(name_node.eval(ctx)?, define_loc);
        let class_name = class.name.clone();
        let class_id = ctx.symbol_map.add_class(class, ctx.current_file_id());
        ctx.scope.add_class(class_name, class_id);

        let parent_class_list = self.record_body()?.eval(ctx)?;
        for (class_id, reference_loc) in parent_class_list {
            ctx.symbol_map.add_class_reference(class_id, reference_loc);
        }
        Some(())
    }
}

impl Eval for ast::RecordBody {
    type Output = Vec<(ClassId, FileRange)>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.parent_class_list()?.eval(ctx)
    }
}

impl Eval for ast::ParentClassList {
    type Output = Vec<(ClassId, FileRange)>;
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        Some(self.classes().filter_map(|node| node.eval(ctx)).collect())
    }
}

impl Eval for ast::ClassRef {
    type Output = (ClassId, FileRange);
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let class_name = self.name()?.value()?;
        let Some(class_id) = ctx.scope.find_class(&class_name) else {
            ctx.error(self.syntax().text_range(), "class not found: {class_name}");
            return None;
        };
        let range = FileRange::new(ctx.current_file_id(), self.syntax().text_range());
        Some((class_id, range))
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
                eco_format!("include file not found: {path}"),
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

impl Eval for ast::Identifier {
    type Output = EcoString;
    fn eval(self, _ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.value()
    }
}
