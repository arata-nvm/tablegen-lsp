use std::sync::Arc;

use ecow::EcoString;

use syntax::ast;
use syntax::ast::AstNode;
use syntax::ast::SourceFile;

use crate::db::SourceDatabase;
use crate::file::FileId;
use crate::symbol_map::{Class, SymbolMap};

#[salsa::query_group(EvalDatabaseStorage)]
pub trait EvalDatabase: SourceDatabase {
    fn symbol_map(&self, file_id: FileId) -> Arc<SymbolMap>;
}

fn symbol_map(db: &dyn EvalDatabase, file_id: FileId) -> Arc<SymbolMap> {
    let parse = db.parse(file_id);
    let source_file = ast::SourceFile::cast(parse.syntax_node()).unwrap();
    Arc::new(eval(source_file))
}

fn eval(source_file: SourceFile) -> SymbolMap {
    let mut ctx = EvalCtx::new();
    source_file.eval(&mut ctx);
    ctx.symbol_map
}

#[derive(Debug, Default)]
pub struct EvalCtx {
    symbol_map: SymbolMap,
}

impl EvalCtx {
    pub fn new() -> Self {
        Self::default()
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
