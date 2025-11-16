pub mod bang_operator;
pub mod context;
pub mod dump;
pub mod scope;

use context::IndexCtx;
use ecow::EcoString;
use scope::ScopeKind;
use std::{collections::HashSet, sync::Arc};
use syntax::{
    SyntaxNodePtr,
    ast::{self, AstNode},
    parser::TextRange,
};

use crate::{
    TY,
    db::SourceDatabase,
    file_system::{FilePosition, FileRange, IncludeId, SourceUnitId},
    handlers::diagnostics::Diagnostic,
    interop::TblgenSymbolTable,
    symbol_map::{
        SymbolMap,
        class::{Class, ClassId},
        def::Def,
        defm::Defm,
        defset::Defset,
        multiclass::{Multiclass, MulticlassId},
        record::RecordField,
        symbol::{Symbol, SymbolId},
        template_arg::TemplateArgument,
        typ::Type,
        variable::{Variable, VariableId, VariableKind},
    },
};

#[salsa::query_group(IndexDatabaseStorage)]
pub trait IndexDatabase: SourceDatabase {
    fn index(&self, source_unit_id: SourceUnitId) -> Arc<Index>;
}

#[derive(Debug, Eq, PartialEq)]
pub struct Index {
    symbol_map: SymbolMap,
    diagnostics: Vec<Diagnostic>,
}

impl Index {
    pub fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

fn index(db: &dyn IndexDatabase, source_unit_id: SourceUnitId) -> Arc<Index> {
    let source_unit = db.source_unit(source_unit_id);

    let parse = db.parse(source_unit.root());
    let source_file =
        ast::SourceFile::cast(parse.syntax_node()).expect("failed to SourceFile::cast");

    let symtab = match db.tblgen_symbol_table(source_unit_id) {
        Some(symtab) => symtab,
        None => Arc::new(TblgenSymbolTable::default()),
    };

    let mut ctx = IndexCtx::new(db, source_unit, symtab);
    source_file.index_statement(&mut ctx);
    Arc::new(ctx.finish())
}

const PLACEHOLDER_DEF_NAME: &str = "placeholder";

pub trait IndexStatement {
    fn index_statement(&self, ctx: &mut IndexCtx);
}

pub trait IndexExpression {
    type Output;
    #[must_use]
    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output>;
}

trait IndexValue {
    type Output;
    #[must_use]
    fn index_value(&self, ctx: &mut IndexCtx, mode: IndexValueMode) -> Option<Self::Output>;
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IndexValueMode {
    Expression,
    Name,
}

impl IndexStatement for ast::SourceFile {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(statement_list) = self.statement_list() {
            statement_list.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::StatementList {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        for statement in self.statements() {
            statement.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::Statement {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        match self {
            ast::Statement::Include(include) => include.index_statement(ctx),
            ast::Statement::Assert(assert) => assert.index_statement(ctx),
            ast::Statement::Class(class) => class.index_statement(ctx),
            ast::Statement::Def(def) => def.index_statement(ctx),
            ast::Statement::Defm(defm) => defm.index_statement(ctx),
            ast::Statement::Defset(defset) => defset.index_statement(ctx),
            ast::Statement::Defvar(defvar) => defvar.index_statement(ctx),
            ast::Statement::Dump(dump) => dump.index_statement(ctx),
            ast::Statement::Foreach(foreach) => foreach.index_statement(ctx),
            ast::Statement::If(r#if) => r#if.index_statement(ctx),
            ast::Statement::Let(r#let) => r#let.index_statement(ctx),
            ast::Statement::MultiClass(multiclass) => multiclass.index_statement(ctx),
        }
    }
}

impl IndexStatement for ast::Include {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let file_id = ctx.current_file_id();
        let Some(include_map) = ctx.source_unit.include_map(&file_id) else {
            tracing::warn!("include map not found for file_id {:?}", file_id);
            return;
        };

        let include_id = IncludeId(SyntaxNodePtr::new(self.syntax()));
        let Some(include_file_id) = include_map.get(&include_id).copied() else {
            let path = self.path().map(|it| it.value()).unwrap_or_default();
            ctx.error_by_syntax(self.syntax(), format!("include file not found: {path}"));
            return;
        };

        let parse = ctx.db.parse(include_file_id);
        let Some(source_file) = ast::SourceFile::cast(parse.syntax_node()) else {
            return;
        };

        ctx.push_file(include_file_id);
        source_file.index_statement(ctx);
        ctx.pop_file();
    }
}

impl IndexStatement for ast::Assert {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(message) = self.message() {
            let _ = message.index_expression(ctx);
        }
        if let Some(condition) = self.condition() {
            let _ = condition.index_expression(ctx);
        }
    }
}

// TODO: check if class is already defined
impl IndexStatement for ast::Class {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, define_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };
        let class = Class::new(name, define_loc);

        let class_id = match ctx.symbol_map.add_class(class) {
            Ok(class_id) => class_id,
            Err(err) => {
                ctx.error_by_filerange(define_loc, err.to_string());
                return;
            }
        };

        ctx.scopes.push(ScopeKind::Class(class_id));
        if let Some(list) = self.template_arg_list() {
            list.index_statement(ctx);
        }
        if let Some(body) = self.record_body() {
            body.index_statement(ctx);
        }
        ctx.scopes.pop();
    }
}

// TODO: check if def is already defined
impl IndexStatement for ast::Def {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(multiclass_id) = ctx.scopes.current_multiclass_id() {
            index_multiclass_def(self, ctx, multiclass_id);
        } else {
            index_global_def(self, ctx);
        }
    }
}

fn index_multiclass_def(def: &ast::Def, ctx: &mut IndexCtx, multiclass_id: MulticlassId) {
    let Some(def_name_type) = determine_def_type(def) else {
        return;
    };

    let def_kw_loc = def
        .syntax()
        .first_token()
        .expect("def should have Def keyword")
        .text_range();
    let tblgen_define_loc = match def_name_type {
        DefNameType::Identifier(ref name_value, _)
        | DefNameType::ValueStartWithIdentifier(ref name_value) => name_value.syntax().text_range(),
        _ => def_kw_loc,
    };
    let tblgen_define_pos = FilePosition::new(ctx.current_file_id(), tblgen_define_loc.start());

    // FIME: DefIdが必要なため一時的にプレースホルダーを作成しているが、後に参照されてしまう可能性がある
    let define_loc = FileRange::new(ctx.current_file_id(), def_kw_loc);
    let multiclass_def = Def::new(EcoString::from(PLACEHOLDER_DEF_NAME), define_loc);
    let def_id = ctx.symbol_map.add_temporary_def(multiclass_def);
    ctx.add_multiclass_def(tblgen_define_pos, def_id);

    ctx.scopes.push(ScopeKind::Def(def_id));
    if let Some(record_body) = def.record_body() {
        record_body.index_statement(ctx);
    }
    ctx.scopes.pop();

    let multiclass = ctx.symbol_map.multiclass_mut(multiclass_id);
    multiclass.add_def(def_id);
}

fn index_global_def(def: &ast::Def, ctx: &mut IndexCtx) {
    let Some((def_names, define_loc, is_anonymous)) = lookup_def_name(def, ctx) else {
        return;
    };
    if def_names.is_empty() {
        return;
    }

    let mut names_iter = def_names.into_iter();

    let base_def = Def::new(
        names_iter.next().expect("def_names is not empty"),
        define_loc,
    );
    let base_def_id = ctx.symbol_map.add_def(base_def, !is_anonymous);

    ctx.scopes.push(ScopeKind::Def(base_def_id));
    if let Some(record_body) = def.record_body() {
        record_body.index_statement(ctx);
    }
    ctx.scopes.pop();

    let mut def_ids = vec![base_def_id];
    for name in names_iter {
        let base_def = ctx.symbol_map.def(base_def_id);
        let mut cloned_def = base_def.clone();
        cloned_def.name = name;
        let def_id = ctx.symbol_map.add_def(cloned_def, !is_anonymous);
        def_ids.push(def_id);
    }

    if let Some(defset_id) = ctx.scopes.current_defset_id() {
        let defset = ctx.symbol_map.defset_mut(defset_id);
        defset.add_defs(def_ids);
    }
}

fn lookup_def_name(
    def: &ast::Def,
    ctx: &mut IndexCtx,
) -> Option<(Vec<EcoString>, FileRange, bool)> {
    let def_name_type = determine_def_type(def)?;

    let def_kw_loc = def
        .syntax()
        .first_token()
        .expect("def should have Def keyword")
        .text_range();
    let tblgen_define_loc = match def_name_type {
        DefNameType::Identifier(ref name_value, _)
        | DefNameType::ValueStartWithIdentifier(ref name_value) => name_value.syntax().text_range(),
        _ => def_kw_loc,
    };
    let tblgen_define_pos = FilePosition::new(ctx.current_file_id(), tblgen_define_loc.start());

    let tblgen_defs = ctx.get_tblgen_defs_at(&tblgen_define_pos);
    if tblgen_defs.is_empty() {
        return match def_name_type {
            DefNameType::Identifier(_, ref ident) => {
                // fallback
                let (name, define_loc) = utils::identifier(ident, ctx)?;
                Some((vec![name], define_loc, false))
            }
            _ => {
                tracing::info!("cannot find tblgen def for def at {:?}", tblgen_define_pos);
                None
            }
        };
    }

    let def_names = tblgen_defs
        .iter()
        .map(|def| def.name.clone())
        .collect::<Vec<_>>();
    let define_loc = FileRange::new(ctx.current_file_id(), tblgen_define_loc);
    let is_anonymous = matches!(def_name_type, DefNameType::Anonymous);
    Some((def_names, define_loc, is_anonymous))
}

#[derive(Debug)]
enum DefNameType {
    // def foo
    Identifier(ast::Value, ast::Identifier),
    // def foo#i
    ValueStartWithIdentifier(ast::Value),
    // def !strconcat(foo, bar)
    Value,
    // def
    Anonymous,
}

fn determine_def_type(def: &ast::Def) -> Option<DefNameType> {
    let Some(name_value) = def.name() else {
        return Some(DefNameType::Anonymous);
    };
    let inner_value = name_value.inner_values().next()?;
    let simple_value = inner_value.simple_value()?;
    match simple_value {
        ast::SimpleValue::Identifier(ident) => {
            if name_value.inner_values().count() > 1 {
                Some(DefNameType::ValueStartWithIdentifier(name_value))
            } else {
                Some(DefNameType::Identifier(name_value, ident))
            }
        }
        _ => Some(DefNameType::Value),
    }
}

impl IndexStatement for ast::Defm {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let defm_kw_loc = self
            .syntax()
            .first_token()
            .expect("defm should have Defm keyword")
            .text_range();
        let define_loc = FileRange::new(ctx.current_file_id(), defm_kw_loc);

        if let Some(multiclass_id) = ctx.scopes.current_multiclass_id() {
            index_multiclass_defm(self, ctx, define_loc, multiclass_id);
        } else {
            index_global_defm(self, ctx, define_loc);
        }
    }
}

fn index_multiclass_defm(
    defm: &ast::Defm,
    ctx: &mut IndexCtx,
    define_loc: FileRange,
    multiclass_id: MulticlassId,
) {
    // FIXME: DefmIdが必要なため一時的にプレースホルダーを作成しているが、後に参照されてしまう可能性がある
    let multiclass_defm = Defm::new(EcoString::from(PLACEHOLDER_DEF_NAME), define_loc);
    let defm_id = ctx.symbol_map.add_temporary_defm(multiclass_defm);

    ctx.scopes.push(ScopeKind::Defm(defm_id));
    if let Some(parent_class_list) = defm.parent_class_list() {
        parent_class_list.index_statement(ctx);
    }
    ctx.scopes.pop();

    let multiclass = ctx.symbol_map.multiclass_mut(multiclass_id);
    multiclass.add_defm(defm_id);
}

fn index_global_defm(defm: &ast::Defm, ctx: &mut IndexCtx, define_loc: FileRange) {
    let Some(parent_class_list) = defm.parent_class_list() else {
        return;
    };
    let superclass_locs = parent_class_list
        .classes()
        .map(|class| class.syntax().text_range().start());

    let mut defs = Vec::new();
    for super_class_loc in superclass_locs {
        let super_class_pos = FilePosition::new(ctx.current_file_id(), super_class_loc);
        defs.extend(ctx.get_tblgen_defs_at(&super_class_pos).iter().cloned());
    }

    if defs.is_empty() {
        return;
    }

    let mut def_ids = Vec::new();
    for def in defs {
        let Some(base_def_id) = ctx.get_multiclass_def_at(&def.define_loc) else {
            continue;
        };
        let base_def = ctx.symbol_map.def(base_def_id);

        let mut cloned_def = base_def.clone();
        cloned_def.name = def.name;
        cloned_def.define_loc = define_loc;
        let def_id = ctx.symbol_map.add_def(cloned_def, defm.name().is_some());
        def_ids.push(def_id);
    }

    if let Some(defset_id) = ctx.scopes.current_defset_id() {
        let defset = ctx.symbol_map.defset_mut(defset_id);
        defset.add_defs(def_ids);
    }
}

impl IndexStatement for ast::Defset {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, define_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };
        let Some(typ) = self.r#type().and_then(|t| t.index_expression(ctx)) else {
            return;
        };
        let defset = Defset::new(name, typ, define_loc);
        let defset_id = ctx.symbol_map.add_defset(defset);

        ctx.scopes.push(ScopeKind::Defset(defset_id));
        if let Some(statement_list) = self.statement_list() {
            statement_list.index_statement(ctx);
        }
        ctx.scopes.pop();
    }
}

impl IndexStatement for ast::Defvar {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, define_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };
        let Some(typ) = self.value().and_then(|v| v.index_expression(ctx)) else {
            return;
        };
        let variable = Variable::new(name, typ, VariableKind::Defvar, define_loc);
        ctx.scopes.add_variable(&mut ctx.symbol_map, variable);
    }
}

impl IndexStatement for ast::Dump {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(value) = self.value() {
            let _ = value.index_expression(ctx);
        }
    }
}

impl IndexStatement for ast::Foreach {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(iterator) = self.iterator() else {
            return;
        };
        let Some((name, variable_id)) = index_declaration(&iterator, ctx) else {
            return;
        };

        ctx.scopes.push(ScopeKind::Foreach(name, variable_id));
        if let Some(body) = self.body() {
            body.index_statement(ctx);
        }
        ctx.scopes.pop();
    }
}

fn index_declaration(
    this: &ast::ForeachIterator,
    ctx: &mut IndexCtx,
) -> Option<(EcoString, VariableId)> {
    let (name, define_loc) = utils::identifier(&this.name()?, ctx)?;
    let typ = this.init()?.index_expression(ctx)?;

    let variable = Variable::new(name.clone(), typ, VariableKind::Foreach, define_loc);
    let variable_id = ctx.symbol_map.add_variable(variable);

    Some((name, variable_id))
}

impl IndexExpression for ast::ForeachIteratorInit {
    type Output = Type;

    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            Self::RangeList(range_list) => {
                for piece in range_list.pieces() {
                    if let Some(start) = piece.start() {
                        let _ = start.index_expression(ctx);
                    }
                    if let Some(end) = piece.end() {
                        let _ = end.index_expression(ctx);
                    }
                }
                Some(TY![int])
            }
            Self::RangePiece(range_piece) => match (range_piece.start(), range_piece.end()) {
                (Some(start), Some(end)) => {
                    let start_typ = start.index_expression(ctx)?;
                    let end_typ = end.index_expression(ctx)?;
                    if start_typ == TY![int] && end_typ == TY![int] {
                        Some(TY![int])
                    } else if start_typ != TY![int] {
                        ctx.error_by_syntax(start.syntax(), "expected integer or bitrange");
                        None
                    } else {
                        // end_typ != TY![int]
                        ctx.error_by_syntax(end.syntax(), "expected integer value as end of range");
                        None
                    }
                }
                (Some(start), None) => {
                    let start_typ = start.index_expression(ctx)?;
                    if start_typ.is_list() {
                        Some(
                            start_typ
                                .element_typ()
                                .expect("list should have element type"),
                        )
                    } else {
                        ctx.error_by_syntax(
                            start.syntax(),
                            format!("expected a list, got '{start_typ}'"),
                        );
                        None
                    }
                }
                _ => None,
            },
            Self::Value(value) => value.index_expression(ctx).and_then(|it| it.element_typ()),
        }
    }
}

impl IndexStatement for ast::If {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(condition) = self.condition() {
            let _ = condition.index_expression(ctx);
        }
        if let Some(then_body) = self.then_body() {
            then_body.index_statement(ctx);
        }
        if let Some(else_body) = self.else_body() {
            else_body.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::Let {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(let_list) = self.let_list() {
            let_list.index_statement(ctx);
        }
        if let Some(statement_list) = self.statement_list() {
            statement_list.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::LetList {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        for let_item in self.items() {
            let_item.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::LetItem {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(value) = self.value() {
            let _ = value.index_expression(ctx);
        }
    }
}

impl IndexStatement for ast::MultiClass {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, define_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };
        let multiclass = Multiclass::new(name, define_loc);
        let multiclass_id = ctx.symbol_map.add_multiclass(multiclass);

        ctx.scopes.push(ScopeKind::Multiclass(multiclass_id));
        if let Some(template_arg_list) = self.template_arg_list() {
            template_arg_list.index_statement(ctx);
        }
        if let Some(parent_class_list) = self.parent_class_list() {
            parent_class_list.index_statement(ctx);
        }
        if let Some(statement_list) = self.statement_list() {
            statement_list.index_statement(ctx);
        }
        ctx.scopes.pop();
    }
}

impl IndexStatement for ast::TemplateArgList {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        for template_arg in self.args() {
            template_arg.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::TemplateArgDecl {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, define_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };
        let Some(typ) = self.r#type().and_then(|it| it.index_expression(ctx)) else {
            return;
        };
        let has_default_value = self.value().is_some();
        let template_arg = TemplateArgument::new(name.clone(), typ, has_default_value, define_loc);
        let template_arg_id = ctx.symbol_map.add_template_argument(template_arg);

        if let Some(class_id) = ctx.scopes.current_class_id() {
            let class = ctx.symbol_map.class_mut(class_id);
            class.add_template_arg(name, template_arg_id);
        } else if let Some(multiclass_id) = ctx.scopes.current_multiclass_id() {
            let multiclass = ctx.symbol_map.multiclass_mut(multiclass_id);
            multiclass.add_template_arg(name, template_arg_id);
        } else {
            tracing::warn!("template arg decl outside of record or multiclass");
        }

        if let Some(value) = self.value() {
            let _ = value.index_expression(ctx);
        }
    }
}

impl IndexStatement for ast::RecordBody {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(parent_class_list) = self.parent_class_list() {
            parent_class_list.index_statement(ctx);
        }
        if let Some(body) = self.body() {
            body.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::ParentClassList {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        if let Some(record_id) = ctx.scopes.current_record_id() {
            for class_ref in self.classes() {
                if let Some(class_id) = resolve_class_ref_as_class(&class_ref, ctx)
                    && let Err(e) = ctx.symbol_map.add_parent_to_record(record_id, class_id)
                {
                    ctx.error_by_syntax(class_ref.syntax(), e.to_string());
                }
            }
        } else if let Some(multiclass_id) = ctx.scopes.current_multiclass_id() {
            for class_ref in self.classes() {
                if let Some(parent_multiclass_id) = resolve_class_ref_as_multiclass(&class_ref, ctx)
                {
                    let multiclass = ctx.symbol_map.multiclass_mut(multiclass_id);
                    multiclass.add_parent(parent_multiclass_id);
                }
            }
        } else if let Some(defm_id) = ctx.scopes.current_defm_id() {
            for class_ref in self.classes() {
                if let Some(parent_multiclass_id) = resolve_class_ref_as_multiclass(&class_ref, ctx)
                {
                    let defm = ctx.symbol_map.defm_mut(defm_id);
                    defm.add_parent(parent_multiclass_id);
                }
            }
        } else {
            tracing::warn!("parent class list outside of record or multiclass");
        }
    }
}

fn resolve_class_ref_as_class(class_ref: &ast::ClassRef, ctx: &mut IndexCtx) -> Option<ClassId> {
    let (name, reference_loc) = utils::identifier(&class_ref.name()?, ctx)?;
    let Some(class_id) = ctx.symbol_map.find_class(&name) else {
        ctx.error_by_filerange(reference_loc, format!("class not found: {name}"));
        return None;
    };
    ctx.symbol_map.add_reference(class_id, reference_loc);

    let class = ctx.symbol_map.class(class_id);
    let template_args = class
        .iter_template_arg()
        .map(|id| ctx.symbol_map.template_arg(id))
        .cloned()
        .collect();

    let arg_values = class_ref
        .arg_value_list()
        .and_then(|it| it.index_expression(ctx))
        .unwrap_or_default();

    check_template_args(
        ctx,
        template_args,
        arg_values,
        class_ref.syntax().text_range(),
    );

    Some(class_id)
}

fn resolve_class_ref_as_multiclass(
    class_ref: &ast::ClassRef,
    ctx: &mut IndexCtx,
) -> Option<MulticlassId> {
    let (name, reference_loc) = utils::identifier(&class_ref.name()?, ctx)?;
    let Some(multiclass_id) = ctx.symbol_map.find_multiclass(&name) else {
        ctx.error_by_filerange(reference_loc, format!("multiclass not found: {name}"));
        return None;
    };
    ctx.symbol_map.add_reference(multiclass_id, reference_loc);

    let multiclass = ctx.symbol_map.multiclass(multiclass_id);
    let template_args = multiclass
        .iter_template_arg()
        .map(|id| ctx.symbol_map.template_arg(id))
        .cloned()
        .collect();

    let arg_values = class_ref
        .arg_value_list()
        .and_then(|it| it.index_expression(ctx))
        .unwrap_or_default();

    check_template_args(
        ctx,
        template_args,
        arg_values,
        class_ref.syntax().text_range(),
    );

    Some(multiclass_id)
}

fn check_template_args(
    ctx: &mut IndexCtx,
    template_args: Vec<TemplateArgument>,
    arg_values: Vec<Option<(Option<EcoString>, Type, TextRange)>>,
    range: TextRange,
) {
    if arg_values.len() > template_args.len() {
        ctx.error_by_textrange(range, format!("too many arguments: {}", arg_values.len()));
        return;
    }

    let mut unsolved_args: HashSet<EcoString> =
        template_args.iter().map(|arg| arg.name.clone()).collect();

    for (idx, arg_value) in arg_values.into_iter().enumerate() {
        let arg = template_args.get(idx).unwrap();
        let Some((arg_value_name, arg_value_typ, arg_value_range)) = arg_value else {
            unsolved_args.remove(&arg.name);
            continue;
        };

        let arg_name_typ = match arg_value_name {
            None => {
                unsolved_args.remove(&arg.name);
                Some((&arg.name, &arg.typ))
            }
            Some(arg_value_name) => {
                let arg = template_args.iter().find(|arg| arg.name == arg_value_name);
                if unsolved_args.remove(&arg_value_name) {
                    arg.map(|it| (&it.name, &it.typ))
                } else if arg.is_some() {
                    ctx.error_by_textrange(
                        arg_value_range,
                        format!(
                            "we can only specify the template argument '{arg_value_name}' once"
                        ),
                    );
                    None
                } else {
                    ctx.error_by_textrange(
                        arg_value_range,
                        format!("argument '{arg_value_name}' doesn't exist"),
                    );
                    None
                }
            }
        };

        if let Some((arg_name, arg_typ)) = arg_name_typ
            && !arg_value_typ.can_be_casted_to(&ctx.symbol_map, arg_typ)
        {
            ctx.error_by_textrange(
    				arg_value_range,
    				format!(
    					"value specified for template argument '{arg_name}' is type of {arg_value_typ}; expected type {arg_typ}"
    				),
    			);
        }
    }

    for unsolved_arg in unsolved_args {
        let arg = template_args.iter().find(|arg| arg.name == unsolved_arg);
        if let Some(arg) = arg
            && !arg.has_default_value
        {
            ctx.error_by_textrange(
                range,
                format!("value not specified for template argument '{unsolved_arg}'"),
            );
        }
    }
}

impl IndexExpression for ast::ArgValueList {
    type Output = Vec<Option<(Option<EcoString>, Type, TextRange)>>;

    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let result = self
            .arg_values()
            .map(|arg_value| arg_value.index_expression(ctx))
            .collect();
        Some(result)
    }
}

impl IndexExpression for ast::ArgValue {
    type Output = (Option<EcoString>, Type, TextRange);

    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            ast::ArgValue::PositionalArgValue(positional) => {
                let typ = positional.value()?.index_expression(ctx)?;
                Some((None, typ, positional.syntax().text_range()))
            }
            ast::ArgValue::NamedArgValue(named) => {
                let ast::SimpleValue::String(name) =
                    named.name()?.inner_values().next()?.simple_value()?
                else {
                    ctx.error_by_syntax(
                        named.syntax(),
                        "the name of named argument should be a valid identifier",
                    );
                    return None;
                };
                let typ = named.value()?.index_expression(ctx)?;
                Some((Some(name.value()), typ, named.syntax().text_range()))
            }
        }
    }
}

impl IndexStatement for ast::Body {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        for item in self.items() {
            item.index_statement(ctx);
        }
    }
}

impl IndexStatement for ast::BodyItem {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        match self {
            ast::BodyItem::FieldDef(field_def) => field_def.index_statement(ctx),
            ast::BodyItem::FieldLet(field_let) => field_let.index_statement(ctx),
            ast::BodyItem::Assert(assert) => assert.index_statement(ctx),
            ast::BodyItem::Defvar(defvar) => defvar.index_statement(ctx),
            ast::BodyItem::Dump(dump) => dump.index_statement(ctx),
        }
    }
}

// TODO: check if field is already defined
impl IndexStatement for ast::FieldDef {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let record_id = ctx
            .scopes
            .current_record_id()
            .expect("field def outside of record");

        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, define_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };
        let Some(typ) = self.r#type().and_then(|t| t.index_expression(ctx)) else {
            return;
        };
        let field = RecordField::new(name.clone(), typ.clone(), record_id, define_loc);
        let field_id = ctx.symbol_map.add_record_field(field);

        let mut record = ctx.symbol_map.record_mut(record_id);
        record.add_record_field(name.clone(), field_id);

        let Some(value) = self.value() else {
            return;
        };
        let Some(value_typ) = value.index_expression(ctx) else {
            return;
        };
        if !value_typ.can_be_casted_to(&ctx.symbol_map, &typ) {
            ctx.error_by_syntax(
                value.syntax(),
                format!("field '{name}' of type '{typ}' is incompatible with type '{value_typ}'",),
            );
        }
    }
}

// TODO: check if field is already defined
impl IndexStatement for ast::FieldLet {
    fn index_statement(&self, ctx: &mut IndexCtx) {
        let Some(name_node) = self.name() else {
            return;
        };
        let Some((name, reference_loc)) = utils::identifier(&name_node, ctx) else {
            return;
        };

        let record_id = ctx
            .scopes
            .current_record_id()
            .expect("field let outside of record");
        let record = ctx.symbol_map.record(record_id);

        let Some(field_id) = record.find_field(&ctx.symbol_map, &name) else {
            ctx.error_by_syntax(self.syntax(), format!("field '{name}' not found in record"));
            return;
        };
        let field = ctx.symbol_map.record_field(field_id);
        let mut field_typ = field.typ.clone();

        let new_field = RecordField::new(name.clone(), field_typ.clone(), record_id, reference_loc);
        let new_field_id = ctx.symbol_map.add_record_field(new_field);

        let mut record = ctx.symbol_map.record_mut(record_id);
        record.add_record_field(name.clone(), new_field_id);
        ctx.symbol_map.add_reference(field_id, reference_loc);

        let Some(value) = self.value() else {
            return;
        };
        let Some(value_typ) = value.index_expression(ctx) else {
            return;
        };

        if let Some(range_list) = self.range_list()
            && let Some(bits) = get_bit_list_in_range_list(&range_list)
        {
            match field_typ.with_bits(bits) {
                Ok(typ) => field_typ = typ,
                Err(err) => {
                    ctx.error_by_syntax(range_list.syntax(), err.to_string());
                }
            }
        }

        if !value_typ.can_be_casted_to(&ctx.symbol_map, &field_typ) {
            ctx.error_by_syntax(
                value.syntax(),
                format!(
                    "field '{name}' of type '{field_typ}' is incompatible with type '{value_typ}'",
                ),
            );
        }
    }
}

fn get_bit_list_in_range_list(range_list: &ast::RangeList) -> Option<Vec<usize>> {
    let mut bits = Vec::new();
    for piece in range_list.pieces() {
        let piece_bits = get_bit_list_in_range_piece(&piece)?;
        bits.extend(piece_bits);
    }
    Some(bits)
}

fn get_bit_list_in_range_piece(range_piece: &ast::RangePiece) -> Option<Vec<usize>> {
    match (range_piece.start(), range_piece.end()) {
        (Some(start), Some(end)) => {
            let start = value_as_integer(start)?;
            let end = value_as_integer(end)?;
            if start <= end {
                Some((start..=end).map(|v| v as usize).collect())
            } else {
                Some((end..=start).map(|v| v as usize).collect())
            }
        }
        (Some(start), None) => {
            let bit = value_as_integer(start)?;
            Some(vec![bit as usize])
        }
        _ => None,
    }
}

// ValueがIntegerの場合、その値を返す
fn value_as_integer(value: ast::Value) -> Option<i64> {
    let mut inner_values_iter = value.inner_values();
    let Some(inner_value) = inner_values_iter.next() else {
        return None;
    };

    if inner_values_iter.next().is_some() {
        return None;
    }

    if inner_value.suffixes().count() > 0 {
        return None;
    }

    let Some(ast::SimpleValue::Integer(integer)) = inner_value.simple_value() else {
        return None;
    };

    integer.value()
}

impl IndexExpression for ast::Value {
    type Output = Type;

    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let mut inner_values = self.inner_values();

        let first_value = inner_values.next()?;
        let first_value_typ = first_value.index_value(ctx, IndexValueMode::Expression);
        let mut ret_typ = first_value_typ.clone();
        for inner_value in inner_values {
            if let Some(inner_value_typ) = inner_value.index_value(ctx, IndexValueMode::Name) {
                if inner_value_typ.can_be_casted_to(&ctx.symbol_map, &TY![string]) {
                    ret_typ = Some(TY![string]);
                } else if ret_typ != Some(TY![string]) && inner_value_typ.is_list() {
                    ret_typ = Some(inner_value_typ);
                }
            }
        }

        ret_typ
    }
}

impl IndexValue for ast::InnerValue {
    type Output = Type;

    fn index_value(&self, ctx: &mut IndexCtx, mode: IndexValueMode) -> Option<Self::Output> {
        let mut lhs_typ = self.simple_value()?.index_value(ctx, mode)?;
        for suffix in self.suffixes() {
            lhs_typ = match suffix {
                ast::ValueSuffix::RangeSuffix(_) => match lhs_typ {
                    Type::Bits(_) => Some(Type::Bit),
                    _ => None,
                },
                ast::ValueSuffix::SliceSuffix(slice_suffix) => {
                    if slice_suffix.is_single_element() {
                        lhs_typ.element_typ()
                    } else {
                        Some(lhs_typ)
                    }
                }
                ast::ValueSuffix::FieldSuffix(field_suffix) => {
                    let (name, reference_loc) = utils::identifier(&field_suffix.name()?, ctx)?;
                    let Some(field_id) = lhs_typ.find_field(&ctx.symbol_map, &name) else {
                        ctx.error_by_syntax(
                            field_suffix.syntax(),
                            format!("cannot access field: {name}"),
                        );
                        return None;
                    };
                    ctx.symbol_map.add_reference(field_id, reference_loc);
                    let field = ctx.symbol_map.record_field(field_id);
                    Some(field.typ.clone())
                }
            }?;
        }
        Some(lhs_typ)
    }
}

impl IndexValue for ast::SimpleValue {
    type Output = Type;

    fn index_value(&self, ctx: &mut IndexCtx, mode: IndexValueMode) -> Option<Self::Output> {
        match self {
            ast::SimpleValue::Integer(_) => Some(Type::Int),
            ast::SimpleValue::String(_) => Some(Type::String),
            ast::SimpleValue::Code(_) => Some(Type::Code),
            ast::SimpleValue::Boolean(_) => Some(Type::Bit),
            ast::SimpleValue::Uninitialized(_) => Some(Type::Uninitialized),
            ast::SimpleValue::Bits(bits) => {
                for value in bits.value_list()?.values() {
                    let _ = value.index_expression(ctx);
                }
                Some(Type::Bits(bits.value_list()?.values().count()))
            }
            ast::SimpleValue::List(list) => {
                let value_list = list.value_list()?;
                let value_types: Vec<_> = value_list
                    .values()
                    .filter_map(|value| value.index_expression(ctx))
                    .collect();
                value_types
                    .into_iter()
                    .next()
                    .map(|typ| Type::List(Box::new(typ)))
                    .or(Some(Type::List(Box::new(Type::Any))))
            }
            ast::SimpleValue::Dag(dag) => {
                if let Some(value) = dag.operator().and_then(|it| it.value()) {
                    let _ = value.index_expression(ctx);
                }
                if let Some(arg_list) = dag.arg_list() {
                    for value in arg_list.args().filter_map(|it| it.value()) {
                        let _ = value.index_expression(ctx);
                    }
                }
                Some(Type::Dag)
            }
            ast::SimpleValue::Identifier(identifier) => {
                let (name, reference_loc) = utils::identifier(identifier, ctx)?;
                if name == "NAME" {
                    return Some(Type::String);
                }
                let Some(symbol_id) = ctx.resolve_id(&name) else {
                    return if mode == IndexValueMode::Name {
                        Some(Type::String)
                    } else {
                        ctx.error_by_filerange(reference_loc, format!("symbol not found: {name}"));
                        None
                    };
                };
                ctx.symbol_map.add_reference(symbol_id, reference_loc);

                if let SymbolId::DefId(def_id) = symbol_id {
                    return Some(Type::Record(def_id.into(), name));
                };
                match ctx.symbol_map.symbol(symbol_id) {
                    Symbol::Class(_) => None,
                    Symbol::Def(_) => unreachable!(),
                    Symbol::TemplateArgument(template_arg) => Some(template_arg.typ.clone()),
                    Symbol::RecordField(field) => Some(field.typ.clone()),
                    Symbol::Variable(variable) => Some(variable.typ.clone()),
                    Symbol::Defset(defset) => Some(defset.typ.clone()),
                    Symbol::Multiclass(_) => None,
                    Symbol::Defm(_) => None,
                }
            }
            ast::SimpleValue::ClassValue(class_value) => {
                let (name, reference_loc) = utils::identifier(&class_value.name()?, ctx)?;
                let class_id = ctx.symbol_map.find_class(&name)?;
                ctx.symbol_map.add_reference(class_id, reference_loc);

                let class = ctx.symbol_map.class(class_id);
                let template_args = class
                    .iter_template_arg()
                    .map(|id| ctx.symbol_map.template_arg(id))
                    .cloned()
                    .collect();

                let arg_values = class_value
                    .arg_value_list()
                    .and_then(|it| it.index_expression(ctx))
                    .unwrap_or_default();

                check_template_args(
                    ctx,
                    template_args,
                    arg_values,
                    class_value.syntax().text_range(),
                );

                Some(Type::Record(class_id.into(), name))
            }
            ast::SimpleValue::BangOperator(bang_operator) => bang_operator.index_expression(ctx),
            ast::SimpleValue::CondOperator(cond_operator) => {
                for clause in cond_operator.clauses() {
                    if let Some(condition) = clause.condition() {
                        let _ = condition.index_expression(ctx);
                    }
                    if let Some(value) = clause.value() {
                        let _ = value.index_expression(ctx);
                    }
                }
                None
            }
        }
    }
}

impl IndexExpression for ast::Type {
    type Output = Type;

    fn index_expression(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            ast::Type::BitType(_) => Some(Type::Bit),
            ast::Type::IntType(_) => Some(Type::Int),
            ast::Type::StringType(_) => Some(Type::String),
            ast::Type::CodeType(_) => Some(Type::Code),
            ast::Type::DagType(_) => Some(Type::Dag),
            ast::Type::BitsType(bits) => {
                let len = bits.length()?.value()?;
                Some(Type::Bits(len.try_into().ok()?))
            }
            ast::Type::ListType(list) => {
                let elm_typ = list.inner_type()?.index_expression(ctx)?;
                Some(Type::List(Box::new(elm_typ)))
            }
            ast::Type::ClassId(class_id) => {
                let (name, reference_loc) = utils::identifier(&class_id.name()?, ctx)?;
                match ctx.symbol_map.find_class(&name) {
                    Some(class_id) => {
                        ctx.symbol_map.add_reference(class_id, reference_loc);
                        Some(Type::Record(class_id.into(), name))
                    }
                    None => {
                        ctx.error_by_filerange(reference_loc, format!("class not found: {name}"));
                        Some(Type::NotResolved(name))
                    }
                }
            }
        }
    }
}

mod utils {
    use super::context::IndexCtx;
    use crate::file_system::FileRange;
    use ecow::EcoString;
    use syntax::ast;

    pub(super) fn identifier(
        identifier: &ast::Identifier,
        ctx: &mut IndexCtx,
    ) -> Option<(EcoString, FileRange)> {
        let name = identifier.value()?;
        let loc = FileRange::new(ctx.current_file_id(), identifier.range()?);
        Some((name, loc))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        handlers::diagnostics::Diagnostic,
        index::{IndexDatabase, dump::dump_symbol_map},
        tests,
    };

    fn dump_diagnostics(diags: &[Diagnostic]) -> Vec<Diagnostic> {
        let mut lsp_diags = diags
            .iter()
            .filter_map(|d| match d {
                Diagnostic::Lsp(d) => Some(d),
                _ => None,
            })
            .cloned()
            .collect::<Vec<_>>();
        lsp_diags.sort_by_key(|d| (d.location.range.start(), d.message.clone()));

        let mut tblgen_diags = diags
            .iter()
            .filter_map(|d| match d {
                Diagnostic::Tblgen(d) => Some(d),
                _ => None,
            })
            .cloned()
            .collect::<Vec<_>>();
        tblgen_diags.sort_by_key(|d| (d.line, d.column, d.message.clone()));

        lsp_diags
            .into_iter()
            .map(Diagnostic::Lsp)
            .chain(tblgen_diags.into_iter().map(Diagnostic::Tblgen))
            .collect()
    }

    #[test]
    fn class() {
        let (db, f) = tests::load_single_file("testdata/class.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));

        let (db, f) = tests::load_single_file_with_tblgen("testdata/class.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));
    }

    #[test]
    fn def() {
        let (db, f) = tests::load_single_file("testdata/def.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));

        let (db, f) = tests::load_single_file_with_tblgen("testdata/def.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));
    }

    #[test]
    fn multiclass() {
        let (db, f) = tests::load_single_file("testdata/multiclass.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));

        let (db, f) = tests::load_single_file_with_tblgen("testdata/multiclass.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));
    }

    #[test]
    fn typ() {
        let (db, f) = tests::load_single_file("testdata/type.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));

        let (db, f) = tests::load_single_file_with_tblgen("testdata/type.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));
    }

    #[test]
    fn variables() {
        let (db, f) = tests::load_single_file("testdata/variables.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));

        let (db, f) = tests::load_single_file_with_tblgen("testdata/variables.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));
    }

    #[test]
    fn errors() {
        let (db, f) = tests::load_single_file("testdata/errors.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));

        let (db, f) = tests::load_single_file_with_tblgen("testdata/errors.td");
        let index = db.index(f.source_unit_id());
        insta::assert_snapshot!(dump_symbol_map(index.symbol_map()));
        insta::assert_debug_snapshot!(dump_diagnostics(index.diagnostics()));
    }
}
