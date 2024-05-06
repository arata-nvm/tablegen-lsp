use core::fmt;
use std::sync::Arc;

use ecow::EcoString;

use syntax::ast;
use syntax::ast::AstNode;
use syntax::parser::TextRange;

use crate::db::SourceDatabase;
use crate::file_system::FileId;
use crate::symbol_map::{Class, SymbolMap};

#[salsa::query_group(EvalDatabaseStorage)]
pub trait EvalDatabase: SourceDatabase {
    fn eval(&self, file_id: FileId) -> Arc<Evaluation>;
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

fn eval(db: &dyn EvalDatabase, file_id: FileId) -> Arc<Evaluation> {
    let parse = db.parse(file_id);
    let source_file = ast::SourceFile::cast(parse.syntax_node()).unwrap();

    let mut ctx = EvalCtx::new();
    source_file.eval(&mut ctx);
    Arc::new(ctx.finish())
}

#[derive(Debug, Default)]
pub struct EvalCtx {
    symbol_map: SymbolMap,
    errors: Vec<EvalError>,
}

impl EvalCtx {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn error(&mut self, range: TextRange, message: impl Into<EcoString>) {
        self.errors.push(EvalError::new(range, message));
    }

    pub fn finish(self) -> Evaluation {
        Evaluation {
            symbol_map: self.symbol_map,
            errors: Vec::new(),
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
            _ => unimplemented!(),
        }
    }
}

impl Eval for ast::Class {
    type Output = ();
    fn eval(self, ctx: &mut EvalCtx) -> Option<Self::Output> {
        let name = self.name()?.eval(ctx)?;
        let class = Class::new(name);
        ctx.symbol_map.add_class(class);
        Some(())
    }
}

impl Eval for ast::Identifier {
    type Output = EcoString;
    fn eval(self, _ctx: &mut EvalCtx) -> Option<Self::Output> {
        self.value()
    }
}
