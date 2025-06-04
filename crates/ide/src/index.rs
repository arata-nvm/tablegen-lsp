pub mod bang_operator;
pub mod context;
pub mod scope;

use std::{collections::HashSet, sync::Arc};

use context::IndexCtx;
use ecow::EcoString;
use scope::ScopeKind;
use syntax::{
    SyntaxNodePtr,
    ast::{self, AstNode},
    parser::TextRange,
};

use crate::{
    db::SourceDatabase,
    file_system::{FileRange, IncludeId, SourceUnitId},
    handlers::diagnostics::Diagnostic,
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

    let mut ctx = IndexCtx::new(db, source_unit);
    source_file.index(&mut ctx);
    Arc::new(ctx.finish())
}

pub trait Indexable {
    type Output;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output>;
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

        let class_id = match ctx.symbol_map.add_class(class, true) {
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
        let defset_id = ctx.scopes.current_defset_id();

        let def_id = match self.name() {
            Some(name_value) => {
                let (name, define_loc) = index_name_value(name_value, ctx)?;
                let def = Def::new(name, define_loc);
                ctx.symbol_map.add_def(def, defset_id.is_none())
            }
            None => {
                let name = ctx.next_anonymous_def_name();
                let def = Def::new(
                    name,
                    FileRange::new(ctx.current_file_id(), self.syntax().text_range()),
                );
                ctx.symbol_map.add_anonymous_def(def)
            }
        };

        if let Some(defset_id) = defset_id {
            let defset = ctx.symbol_map.defset_mut(defset_id);
            defset.add_def(def_id);
        }

        ctx.scopes.push(ScopeKind::Def(def_id));
        self.record_body()?.index(ctx);
        ctx.scopes.pop();

        None
    }
}

fn index_name_value(value: ast::Value, ctx: &mut IndexCtx) -> Option<(EcoString, FileRange)> {
    let name = value.inner_values().next()?;
    match name.simple_value()? {
        ast::SimpleValue::Identifier(id) => utils::identifier(&id, ctx),
        _ => None,
    }
}

impl Indexable for ast::Defm {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let defset_id = ctx.scopes.current_defset_id();

        let defm_id = match self.name() {
            Some(name_value) => {
                let (name, define_loc) = index_name_value(name_value, ctx)?;
                let defm = Defm::new(name, define_loc);
                ctx.symbol_map.add_defm(defm, defset_id.is_none())
            }
            None => {
                let name = ctx.next_anonymous_def_name();
                let defm = Defm::new(
                    name,
                    FileRange::new(ctx.current_file_id(), self.syntax().text_range()),
                );
                ctx.symbol_map.add_anonymous_defm(defm)
            }
        };

        ctx.scopes.push(ScopeKind::Defm(defm_id));
        self.parent_class_list()?.index(ctx);
        ctx.scopes.pop();

        None
    }
}

impl Indexable for ast::Defset {
    type Output = ();
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let (name, define_loc) = utils::identifier(&self.name()?, ctx)?;
        let typ = self.r#type()?.index(ctx)?;
        let defset = Defset::new(name, typ, define_loc);
        let defset_id = ctx.symbol_map.add_defset(defset);

        ctx.scopes.push(ScopeKind::Defset(defset_id));
        self.statement_list()?.index(ctx);
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
        self.body()?.index(ctx);
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
            // bacause RangeList and RangePiece have no items to index, we skip them
            Self::RangeList(_) | Self::RangePiece(_) => Some(Type::Int),
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
        self.template_arg_list()?.index(ctx);
        self.parent_class_list()?.index(ctx);
        self.statement_list()?.index(ctx);
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
            tracing::debug!("{} {:?}", line!(), ctx.scopes);
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
                if let Some(class_id) = resolve_class_ref_as_class(&class_ref, ctx) {
                    let mut record = ctx.symbol_map.record_mut(record_id);
                    record.add_parent(class_id);
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
            let start = self.syntax().text_range().start();
            let line_index = ctx.db.line_index(ctx.current_file_id());
            let line = line_index.pos_to_line(start);
            tracing::debug!("{} {line} {:?}", line!(), ctx.scopes);
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

        if let Some((arg_name, arg_typ)) = arg_name_typ {
            if !arg_value_typ.can_be_casted_to(&ctx.symbol_map, arg_typ) {
                ctx.error_by_textrange(
    				arg_value_range,
    				format!(
    					"value specified for template argument '{arg_name}' is type of {arg_value_typ}; expected type {arg_typ}"
    				),
    			);
            }
        }
    }

    for unsolved_arg in unsolved_args {
        let arg = template_args.iter().find(|arg| arg.name == unsolved_arg);
        if let Some(arg) = arg {
            if !arg.has_default_value {
                ctx.error_by_textrange(
                    range,
                    format!("value not specified for template argument '{unsolved_arg}'"),
                );
            }
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
        let first_value_typ = first_value.index(ctx);
        for inner_value in inner_values {
            inner_value.index(ctx);
        }

        match self.inner_values().count() {
            0 => None,
            1 => first_value_typ,
            _ => Some(Type::String),
        }
    }
}

impl Indexable for ast::InnerValue {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
        let mut lhs_typ = self.simple_value()?.index(ctx)?;
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

impl Indexable for ast::SimpleValue {
    type Output = Type;
    fn index(&self, ctx: &mut IndexCtx) -> Option<Self::Output> {
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
                    ctx.error_by_filerange(reference_loc, format!("symbol not found: {name}"));
                    return None;
                };
                ctx.symbol_map.add_reference(symbol_id, reference_loc);

                if let SymbolId::DefId(def_id) = symbol_id {
                    return Some(Type::Record(def_id.into(), name));
                };
                match ctx.symbol_map.symbol(symbol_id) {
                    Symbol::Class(_) => None,
                    Symbol::Def(_) => None, // unreachable
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
