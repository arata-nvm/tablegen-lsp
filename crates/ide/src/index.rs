pub mod bang_operator;
pub mod context;
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
    source_file.index(&mut ctx);
    Arc::new(ctx.finish())
}

pub trait Indexable {
    type Output;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output>;
}

trait IndexableValue {
    type Output;
    fn index(&self, ctx: &mut IndexCtx, mode: IndexValueMode) -> Option<Self::Output>;
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IndexValueMode {
    Expression,
    Name,
}

impl Indexable for ast::SourceFile {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.statement_list()?.index(ctx);
        None
    }
}

impl Indexable for ast::StatementList {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        for statement in self.statements() {
            statement.index(ctx);
        }
        None
    }
}

impl Indexable for ast::Statement {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            ast::Statement::Include(include) => include.index(ctx),
            ast::Statement::Assert(assert) => assert.index(ctx),
            ast::Statement::Class(class) => class.index(ctx),
            ast::Statement::Def(def) => def.index(ctx),
            ast::Statement::Defm(defm) => defm.index(ctx),
            ast::Statement::Defset(defset) => defset.index(ctx),
            ast::Statement::Defvar(defvar) => defvar.index(ctx),
            ast::Statement::Dump(dump) => dump.index(ctx),
            ast::Statement::Foreach(foreach) => foreach.index(ctx),
            ast::Statement::If(r#if) => r#if.index(ctx),
            ast::Statement::Let(r#let) => r#let.index(ctx),
            ast::Statement::MultiClass(multiclass) => multiclass.index(ctx),
        }
    }
}

impl Indexable for ast::Include {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let file_id = ctx.current_file_id();
        let include_map = ctx.source_unit.include_map(&file_id)?;

        let include_id = IncludeId(SyntaxNodePtr::new(self.syntax()));
        let Some(include_file_id) = include_map.get(&include_id).copied() else {
            let path = self.path().map(|it| it.value()).unwrap_or_default();
            ctx.error_by_syntax(self.syntax(), format!("include file not found: {path}"));
            return None;
        };

        let parse = ctx.db.parse(include_file_id);
        let source_file = ast::SourceFile::cast(parse.syntax_node())?;

        ctx.push_file(include_file_id);
        source_file.index(ctx);
        ctx.pop_file();

        None
    }
}

impl Indexable for ast::Assert {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.message()?.index(ctx);
        self.condition()?.index(ctx);
        None
    }
}

// TODO: check if class is already defined
impl Indexable for ast::Class {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let class = Class::new(name, define_loc);

        let class_id = match ctx.symbol_map.add_class(class) {
            Ok(class_id) => class_id,
            Err(err) => {
                ctx.error_by_filerange(define_loc, err.to_string());
                return None;
            }
        };
        ctx.scopes.push(ScopeKind::Class(class_id));
        if let Some(list) = self.template_arg_list() {
            list.index(ctx);
        }
        if let Some(body) = self.record_body() {
            body.index(ctx);
        }
        ctx.scopes.pop();

        None
    }
}

// TODO: check if def is already defined
impl Indexable for ast::Def {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        if let Some(multiclass_id) = ctx.scopes.current_multiclass_id() {
            index_multiclass_def(self, ctx, multiclass_id)
        } else {
            index_global_def(self, ctx)
        }
    }
}

fn index_multiclass_def(
    def: &ast::Def,
    ctx: &mut IndexCtx,
    multiclass_id: MulticlassId,
) -> Option<()> {
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

    let name = EcoString::from("placeholder_multiclass_def");
    let define_loc = FileRange::new(ctx.current_file_id(), def_kw_loc);
    let multiclass_def = Def::new(name, define_loc);
    let def_id = ctx.symbol_map.add_def(multiclass_def, false);
    ctx.add_multiclass_def(tblgen_define_pos, def_id);
    tracing::debug!(
        "NEKO registered multiclass def at {:?} {:?}",
        tblgen_define_pos,
        def_id
    );

    ctx.scopes.push(ScopeKind::Def(def_id));
    if let Some(record_body) = def.record_body() {
        record_body.index(ctx);
    }
    ctx.scopes.pop();

    let multiclass = ctx.symbol_map.multiclass_mut(multiclass_id);
    multiclass.add_def(def_id);

    None
}

fn index_global_def(def: &ast::Def, ctx: &mut IndexCtx) -> Option<()> {
    let (def_names, define_loc, is_anonymous) = lookup_def_name(def, ctx)?;
    if def_names.is_empty() {
        return None;
    }

    let mut names_iter = def_names.into_iter();

    let base_def = Def::new(
        names_iter.next().expect("def_names is not empty"),
        define_loc,
    );
    let base_def_id = ctx.symbol_map.add_def(base_def, !is_anonymous);

    ctx.scopes.push(ScopeKind::Def(base_def_id));
    if let Some(record_body) = def.record_body() {
        record_body.index(ctx);
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
    } else if let Some(multiclass_id) = ctx.scopes.current_multiclass_id() {
        let multiclass = ctx.symbol_map.multiclass_mut(multiclass_id);
        multiclass.add_defs(def_ids);
    }

    None
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
    let Some(inner_value) = name_value.inner_values().next() else {
        return None;
    };
    let Some(simple_value) = inner_value.simple_value() else {
        return None;
    };
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

impl Indexable for ast::Defm {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let defm_kw_loc = self
            .syntax()
            .first_token()
            .expect("defm should have Defm keyword")
            .text_range();
        let define_loc = FileRange::new(ctx.current_file_id(), defm_kw_loc);

        if ctx.scopes.current_multiclass_id().is_some() {
            index_multiclass_defm(self, ctx, define_loc)
        } else {
            index_global_defm(self, ctx, define_loc)
        }
    }
}

fn index_multiclass_defm(
    defm: &ast::Defm,
    ctx: &mut IndexCtx,
    define_loc: FileRange,
) -> Option<()> {
    tracing::debug!("NEKO in multiclass");
    let multiclass_defm = Defm::new(EcoString::from("placeholder"), define_loc);
    let defm_id = ctx
        .symbol_map
        .add_defm(multiclass_defm, defm.name().is_some());

    ctx.scopes.push(ScopeKind::Defm(defm_id));
    if let Some(parent_class_list) = defm.parent_class_list() {
        parent_class_list.index(ctx);
    }
    ctx.scopes.pop();

    None
}

fn index_global_defm(defm: &ast::Defm, ctx: &mut IndexCtx, define_loc: FileRange) -> Option<()> {
    let parent_class_list = defm.parent_class_list()?;
    let superclass_locs = parent_class_list
        .classes()
        .map(|class| class.syntax().text_range().start());

    let mut defs = Vec::new();
    for super_class_loc in superclass_locs {
        let super_class_pos = FilePosition::new(ctx.current_file_id(), super_class_loc);
        defs.extend(ctx.get_tblgen_defs_at(&super_class_pos).iter().cloned());
    }

    tracing::debug!("NEKO defm/global");
    if defs.is_empty() {
        return None;
    }

    let mut def_ids = Vec::new();
    for def in defs {
        tracing::debug!("NEKO defm/def {:?}", def);
        let Some(base_def_id) = ctx.get_multiclass_def_at(&def.define_loc) else {
            continue;
        };
        let base_def = ctx.symbol_map.def(base_def_id);
        tracing::debug!("NEKO defm/base_def {:?}", base_def);

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

    None
}

impl Indexable for ast::Defset {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let typ = self.r#type()?.index(ctx)?;
        let defset = Defset::new(name, typ, define_loc);
        let defset_id = ctx.symbol_map.add_defset(defset);

        ctx.scopes.push(ScopeKind::Defset(defset_id));
        if let Some(statement_list) = self.statement_list() {
            statement_list.index(ctx);
        }
        ctx.scopes.pop();

        None
    }
}

impl Indexable for ast::Defvar {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let typ = self.value()?.index(ctx)?;
        let variable = Variable::new(name, typ, VariableKind::Defvar, define_loc);
        ctx.scopes.add_variable(&mut ctx.symbol_map, variable);
        None
    }
}

impl Indexable for ast::Dump {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.value()?.index(ctx);
        None
    }
}

impl Indexable for ast::Foreach {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, variable_id) = self.iterator()?.index(ctx)?;
        ctx.scopes.push(ScopeKind::Foreach(name, variable_id));
        if let Some(body) = self.body() {
            body.index(ctx);
        }
        ctx.scopes.pop();
        None
    }
}

impl Indexable for ast::ForeachIterator {
    type Output = (EcoString, VariableId);
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let typ = self.init()?.index(ctx)?;

        let variable = Variable::new(name.clone(), typ, VariableKind::Foreach, define_loc);
        let variable_id = ctx.symbol_map.add_variable(variable);

        Some((name, variable_id))
    }
}

impl Indexable for ast::ForeachIteratorInit {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            Self::RangeList(range_list) => {
                for piece in range_list.pieces() {
                    if let Some(start) = piece.start() {
                        start.index(ctx);
                    }
                    if let Some(end) = piece.end() {
                        end.index(ctx);
                    }
                }
                Some(TY![int])
            }
            Self::RangePiece(range_piece) => match (range_piece.start(), range_piece.end()) {
                (Some(start), Some(end)) => {
                    let start_typ = start.index(ctx)?;
                    let end_typ = end.index(ctx)?;
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
                    let start_typ = start.index(ctx)?;
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
            Self::Value(value) => value.index(ctx).and_then(|it| it.element_typ()),
        }
    }
}

impl Indexable for ast::If {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.condition()?.index(ctx);
        self.then_body()?.index(ctx);
        self.else_body()?.index(ctx);
        None
    }
}

impl Indexable for ast::Let {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.let_list()?.index(ctx);
        self.statement_list()?.index(ctx);
        None
    }
}

impl Indexable for ast::LetList {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        for let_item in self.items() {
            let_item.index(ctx);
        }
        None
    }
}

impl Indexable for ast::LetItem {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.value()?.index(ctx);
        None
    }
}

impl Indexable for ast::MultiClass {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let multiclass = Multiclass::new(name, define_loc);
        let multiclass_id = ctx.symbol_map.add_multiclass(multiclass);

        ctx.scopes.push(ScopeKind::Multiclass(multiclass_id));
        if let Some(template_arg_list) = self.template_arg_list() {
            template_arg_list.index(ctx);
        }
        if let Some(parent_class_list) = self.parent_class_list() {
            parent_class_list.index(ctx);
        }
        if let Some(statement_list) = self.statement_list() {
            statement_list.index(ctx);
        }
        ctx.scopes.pop();

        None
    }
}

impl Indexable for ast::TemplateArgList {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        for template_arg in self.args() {
            template_arg.index(ctx);
        }
        None
    }
}

impl Indexable for ast::TemplateArgDecl {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let typ = self.r#type()?.index(ctx)?;
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
            panic!("template arg decl outside of record or multiclass");
        }

        if let Some(value) = self.value() {
            value.index(ctx);
        }

        None
    }
}

impl Indexable for ast::RecordBody {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        self.parent_class_list()?.index(ctx);
        self.body()?.index(ctx);
        None
    }
}

impl Indexable for ast::ParentClassList {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
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
            panic!("parent class list outside of record or multiclass");
        }
        None
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
        .and_then(|it| it.index(ctx))
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
        .and_then(|it| it.index(ctx))
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

impl Indexable for ast::ArgValueList {
    type Output = Vec<Option<(Option<EcoString>, Type, TextRange)>>;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let result = self
            .arg_values()
            .map(|arg_value| arg_value.index(ctx))
            .collect();
        Some(result)
    }
}

impl Indexable for ast::ArgValue {
    type Output = (Option<EcoString>, Type, TextRange);
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            ast::ArgValue::PositionalArgValue(positional) => {
                let typ = positional.value()?.index(ctx)?;
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
                let typ = named.value()?.index(ctx)?;
                Some((Some(name.value()), typ, named.syntax().text_range()))
            }
        }
    }
}

impl Indexable for ast::Body {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        for item in self.items() {
            item.index(ctx);
        }
        None
    }
}

impl Indexable for ast::BodyItem {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            ast::BodyItem::FieldDef(field_def) => field_def.index(ctx),
            ast::BodyItem::FieldLet(field_let) => field_let.index(ctx),
            ast::BodyItem::Assert(assert) => assert.index(ctx),
            ast::BodyItem::Defvar(defvar) => defvar.index(ctx),
            ast::BodyItem::Dump(dump) => dump.index(ctx),
        }
    }
}

// TODO: check if field is already defined
impl Indexable for ast::FieldDef {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let record_id = ctx
            .scopes
            .current_record_id()
            .expect("field def outside of record");

        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let typ = self.r#type()?.index(ctx)?;
        let field = RecordField::new(name.clone(), typ.clone(), record_id, define_loc);
        let field_id = ctx.symbol_map.add_record_field(field);

        let mut record = ctx.symbol_map.record_mut(record_id);
        record.add_record_field(name.clone(), field_id);

        let value_typ = self.value()?.index(ctx)?;
        if !value_typ.can_be_casted_to(&ctx.symbol_map, &typ) {
            ctx.error_by_syntax(
                self.value()?.syntax(),
                format!("field '{name}' of type '{typ}' is incompatible with type '{value_typ}'",),
            );
        }

        None
    }
}

// TODO: check if field is already defined
impl Indexable for ast::FieldLet {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, reference_loc) = utils::identifier(&self.name()?, ctx)?;

        let record_id = ctx
            .scopes
            .current_record_id()
            .expect("field let outside of record");
        let record = ctx.symbol_map.record(record_id);

        let field_id = record.find_field(&ctx.symbol_map, &name)?;
        let field = ctx.symbol_map.record_field(field_id);
        let field_typ = field.typ.clone();

        let new_field = RecordField::new(name.clone(), field_typ.clone(), record_id, reference_loc);
        let new_field_id = ctx.symbol_map.add_record_field(new_field);

        let mut record = ctx.symbol_map.record_mut(record_id);
        record.add_record_field(name.clone(), new_field_id);
        ctx.symbol_map.add_reference(field_id, reference_loc);

        let value_typ = self.value()?.index(ctx)?;
        if !value_typ.can_be_casted_to(&ctx.symbol_map, &field_typ) {
            ctx.error_by_syntax(
                self.value()?.syntax(),
                format!(
                    "field '{name}' of type '{field_typ}' is incompatible with type '{value_typ}'",
                ),
            );
        }

        None
    }
}

impl Indexable for ast::Value {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let mut inner_values = self.inner_values();

        let first_value = inner_values.next()?;
        let first_value_typ = first_value.index(ctx, IndexValueMode::Expression);
        let mut ret_typ = first_value_typ.clone();
        for inner_value in inner_values {
            if let Some(inner_value_typ) = inner_value.index(ctx, IndexValueMode::Name) {
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

impl IndexableValue for ast::InnerValue {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx, mode: IndexValueMode) -> Option<Self::Output> {
        let mut lhs_typ = self.simple_value()?.index(ctx, mode)?;
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

impl IndexableValue for ast::SimpleValue {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx, mode: IndexValueMode) -> Option<Self::Output> {
        match self {
            ast::SimpleValue::Integer(_) => Some(Type::Int),
            ast::SimpleValue::String(_) => Some(Type::String),
            ast::SimpleValue::Code(_) => Some(Type::Code),
            ast::SimpleValue::Boolean(_) => Some(Type::Bit),
            ast::SimpleValue::Uninitialized(_) => Some(Type::Uninitialized),
            ast::SimpleValue::Bits(bits) => {
                for value in bits.value_list()?.values() {
                    value.index(ctx);
                }
                Some(Type::Bits(bits.value_list()?.values().count()))
            }
            ast::SimpleValue::List(list) => {
                let value_list = list.value_list()?;
                let value_types: Vec<_> = value_list
                    .values()
                    .filter_map(|value| value.index(ctx))
                    .collect();
                value_types
                    .into_iter()
                    .next()
                    .map(|typ| Type::List(Box::new(typ)))
                    .or(Some(Type::List(Box::new(Type::Any))))
            }
            ast::SimpleValue::Dag(dag) => {
                if let Some(value) = dag.operator().and_then(|it| it.value()) {
                    value.index(ctx);
                }
                if let Some(arg_list) = dag.arg_list() {
                    for value in arg_list.args().filter_map(|it| it.value()) {
                        value.index(ctx);
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
                    .and_then(|it| it.index(ctx))
                    .unwrap_or_default();

                check_template_args(
                    ctx,
                    template_args,
                    arg_values,
                    class_value.syntax().text_range(),
                );

                Some(Type::Record(class_id.into(), name))
            }
            ast::SimpleValue::BangOperator(bang_operator) => bang_operator.index(ctx),
            ast::SimpleValue::CondOperator(cond_operator) => {
                for clause in cond_operator.clauses() {
                    if let Some(condition) = clause.condition() {
                        condition.index(ctx);
                    }
                    if let Some(value) = clause.value() {
                        value.index(ctx);
                    }
                }
                None
            }
        }
    }
}

impl Indexable for ast::Type {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        match self {
            ast::Type::BitType(_) => Some(Type::Bit),
            ast::Type::IntType(_) => Some(Type::Int),
            ast::Type::StringType(_) => Some(Type::String),
            ast::Type::CodeType(_) => Some(Type::Code),
            ast::Type::DagType(_) => Some(Type::Dag),
            ast::Type::BitsType(bits) => {
                let len = bits.length()?.index(ctx)?;
                Some(Type::Bits(len.try_into().ok()?))
            }
            ast::Type::ListType(list) => {
                let elm_typ = list.inner_type()?.index(ctx)?;
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

impl Indexable for ast::Integer {
    type Output = i64;
    fn index(&self, _: &mut IndexCtx) -> Option<Self::Output> {
        self.value()
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
