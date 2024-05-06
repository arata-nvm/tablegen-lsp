use core::fmt;
use std::sync::Arc;

use ecow::{eco_format, EcoString};

use syntax::ast::AstNode;
use syntax::ast::{self};
use syntax::parser::TextRange;

use crate::db::SourceDatabase;
use crate::file_system::{FileId, SourceRoot};
use crate::symbol_map::{Class, SymbolMap};

#[salsa::query_group(EvalDatabaseStorage)]
pub trait EvalDatabase: SourceDatabase {
    fn eval(&self) -> Arc<Evaluation>;
}

#[derive(Debug, Eq, PartialEq)]
pub struct Evaluation {
    symbol_map: SymbolMap,
    errors: Vec<EvalError>,
}

impl Evaluation {
    pub fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }

    pub fn errors(&self) -> &[EvalError] {
        &self.errors
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EvalError {
    pub range: TextRange,
    pub message: EcoString,
}

impl EvalError {
    pub fn new(range: TextRange, message: impl Into<EcoString>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.range, self.message)
    }
}

fn eval(db: &dyn EvalDatabase) -> Arc<Evaluation> {
    let source_root = db.source_root();

    let parse = db.parse(source_root.root());
    let source_file = ast::SourceFile::cast(parse.syntax_node()).unwrap();

    let mut ctx = EvalCtx::new(db, &*source_root);
    source_file.eval(&mut ctx);
    Arc::new(ctx.finish())
}

pub struct EvalCtx<'a> {
    db: &'a dyn EvalDatabase,
    source_root: &'a SourceRoot,
    file_trace: Vec<FileId>,
    symbol_map: SymbolMap,
    errors: Vec<EvalError>,
}

impl<'a> EvalCtx<'a> {
    pub fn new(db: &'a dyn EvalDatabase, source_root: &'a SourceRoot) -> Self {
        Self {
            db,
            file_trace: vec![source_root.root()],
            source_root,
            symbol_map: SymbolMap::default(),
            errors: Vec::new(),
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

    pub fn find_include_file(&self, include_path: &str) -> Option<(FileId, ast::SourceFile)> {
        let current_file_path = self.source_root.path_for_file(&self.current_file_id())?;
        let current_file_dir = current_file_path.parent()?;

        let include_file_path = current_file_dir.join(include_path);
        let include_file_id = self.source_root.file_for_path(&include_file_path)?;

        let parse = self.db.parse(include_file_id);
        ast::SourceFile::cast(parse.syntax_node()).map(|it| (include_file_id, it))
    }

    pub fn error(&mut self, range: TextRange, message: impl Into<EcoString>) {
        self.errors.push(EvalError::new(range, message));
    }

    pub fn finish(self) -> Evaluation {
        Evaluation {
            symbol_map: self.symbol_map,
            errors: self.errors,
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
        let range = (ctx.current_file_id(), self.syntax().text_range());
        let class = Class::new(name, range);
        ctx.symbol_map.add_class(class, ctx.current_file_id());
        Some(())
    }
}

impl Eval for ast::Include {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let include_path = self.path()?.value();
        if let Some((file_id, source_file)) = ctx.find_include_file(&include_path) {
            ctx.push_file(file_id);
            source_file.eval(ctx);
            ctx.pop_file();
        } else {
            ctx.error(
                self.syntax().text_range(),
                eco_format!("include file not found: {include_path:?}"),
            );
        }
        Some(())
    }
}

impl Eval for ast::Identifier {
    type Output = EcoString;
    fn eval(self, _ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.value()
    }
}
