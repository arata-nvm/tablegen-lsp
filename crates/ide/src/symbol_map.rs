use std::collections::BTreeMap;
use std::fmt::Debug;
use std::ops::DerefMut;
use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use id_arena::{Arena, Id};

use syntax::parser::{TextRange, TextSize};
use syntax::syntax_kind::SyntaxKind;
use thiserror::Error;

use crate::file_system::{FileId, FilePosition, FileRange};

#[derive(Debug, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub template_arg_list: Vec<TemplateArgumentId>,
    pub name_to_field: BTreeMap<EcoString, FieldId>,
    pub parent_class_list: Vec<ClassId>,
}

pub type ClassId = Id<Class>;

impl From<ClassId> for SymbolId {
    fn from(id: ClassId) -> Self {
        SymbolId::ClassId(id)
    }
}

#[derive(Debug, Error)]
pub enum ClassError {
    #[error("unknown field: {0}")]
    UnknownField(EcoString),
    #[error("type of field '{0}' is incompatible: '{1}' and '{2}'")]
    IncompatibleType(EcoString, Type, Type),
}

impl Class {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            template_arg_list: Vec::new(),
            name_to_field: BTreeMap::new(),
            parent_class_list: Vec::new(),
        }
    }

    pub fn add_template_arg(
        &mut self,
        symbol_map: &mut SymbolMap,
        template_arg: TemplateArgument,
    ) -> TemplateArgumentId {
        let id = symbol_map.add_template_argument(template_arg);
        self.template_arg_list.push(id);
        id
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.template_arg_list.iter().copied()
    }

    pub fn add_field(
        &mut self,
        symbol_map: &mut SymbolMap,
        new_field: Field,
    ) -> Result<FieldId, (TextRange, ClassError)> {
        if let Some(old_field_id) = self.name_to_field.get(&new_field.name) {
            let old_field = symbol_map.field(*old_field_id);
            if old_field.typ != new_field.typ {
                return Err((
                    new_field.define_loc.range,
                    ClassError::IncompatibleType(
                        new_field.name.clone(),
                        old_field.typ.clone(),
                        new_field.typ.clone(),
                    ),
                ));
            }
        }

        let name = new_field.name.clone();
        let id = symbol_map.add_field(new_field);
        self.name_to_field.insert(name, id);
        Ok(id)
    }

    pub fn modify_field(
        &mut self,
        symbol_map: &mut SymbolMap,
        name: EcoString,
        value: Expr,
        define_loc: FileRange,
    ) -> Result<FieldId, ClassError> {
        match self.name_to_field.get(&name) {
            Some(old_field_id) => {
                let old_field = symbol_map.field(*old_field_id);
                let new_field = Field::new(
                    name.clone(),
                    old_field.typ.clone(),
                    value,
                    old_field.parent,
                    define_loc,
                );
                let id = symbol_map.add_field(new_field);
                self.name_to_field.insert(name, id);
                Ok(id)
            }
            None => Err(ClassError::UnknownField(name)),
        }
    }

    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_field.get(name).copied()
    }

    pub fn inherit(
        &mut self,
        symbol_map: &mut SymbolMap,
        parent_class_id: ClassId,
        arg_value_list: Vec<Expr>,
        reference_loc: FileRange,
    ) -> Result<(), (TextRange, ClassError)> {
        symbol_map.add_reference(parent_class_id, reference_loc);
        self.parent_class_list.push(parent_class_id);

        let mut new_field_list = Vec::new();
        let parent_class = symbol_map.class(parent_class_id);

        let mut replacement = HashMap::new();
        let mut iter_arg_value_list = arg_value_list.into_iter();
        for i in 0..parent_class.template_arg_list.len() {
            replacement.insert(
                parent_class.template_arg_list[i],
                iter_arg_value_list
                    .next()
                    .unwrap_or(Expr::Simple(SimpleExpr::Uninitialized)),
            );
        }

        for field_id in parent_class.name_to_field.values() {
            let mut new_field = symbol_map.field(*field_id).clone();
            new_field.value = new_field.value.replaced(&replacement);
            new_field_list.push(new_field)
        }

        for new_field in new_field_list {
            self.add_field(symbol_map, new_field)?;
        }

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TemplateArgument {
    pub name: EcoString,
    pub typ: Type,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

pub type TemplateArgumentId = Id<TemplateArgument>;

impl From<TemplateArgumentId> for SymbolId {
    fn from(id: TemplateArgumentId) -> Self {
        SymbolId::TemplateArgumentId(id)
    }
}

impl TemplateArgument {
    pub fn new(name: EcoString, typ: Type, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub name: EcoString,
    pub typ: Type,
    pub value: Expr,
    pub parent: ClassId,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

pub type FieldId = Id<Field>;

impl From<FieldId> for SymbolId {
    fn from(id: FieldId) -> Self {
        SymbolId::FieldId(id)
    }
}

impl Field {
    pub fn new(
        name: EcoString,
        typ: Type,
        value: Expr,
        parent: ClassId,
        define_loc: FileRange,
    ) -> Self {
        Self {
            name,
            typ,
            value,
            parent,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Def {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

pub type DefId = Id<Def>;

impl From<DefId> for SymbolId {
    fn from(id: DefId) -> Self {
        SymbolId::DefId(id)
    }
}

impl Def {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}

pub type Replacement = HashMap<TemplateArgumentId, Expr>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Simple(SimpleExpr),
    // RangeSuffix(Box<Expr>, Vec<Range>),
    // SliceSuffix(Box<Expr>, Vec<Slice>),
    FieldSuffix(Box<Expr>, EcoString, Type),
    // Paste(Box<Expr>, SimpleExpr),
}

impl Expr {
    pub fn replaced(self, replacement: &Replacement) -> Self {
        match self {
            Self::Simple(SimpleExpr::Identifier(
                _,
                Some((SymbolId::TemplateArgumentId(id), _)),
            )) => match replacement.get(&id) {
                Some(expr) => expr.clone(),
                None => self,
            },
            Self::FieldSuffix(expr, field, typ) => {
                Self::FieldSuffix(Box::new(expr.replaced(replacement)), field, typ)
            }
            _ => self,
        }
    }

    pub fn typ(&self) -> Type {
        match self {
            Self::Simple(value) => value.typ(),
            Self::FieldSuffix(_, _, typ) => typ.clone(),
        }
    }
}

// #[derive(Debug, Eq, PartialEq)]
// pub struct Range {
//     start: i64,
//     end: i64,
// }

// #[derive(Debug, Eq, PartialEq)]
// pub struct Slice {
//     start: Expr,
//     end: Expr,
// }

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple(value) => write!(f, "{value}"),
            Self::FieldSuffix(expr, field, _) => write!(f, "{expr}.{field}"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SimpleExpr {
    Uninitialized,
    Boolean(bool),
    Int(i64),
    String(String),
    Code(EcoString),
    Bits(Vec<Expr>),
    List(Vec<Expr>, Type),
    // Dag(DagArg, Vec<DagArg>),
    Identifier(EcoString, Option<(SymbolId, Type)>),
    // ClassValue,
    BangOperator(BangOperatorOp, Vec<Expr>),
    // CondOperator,
}

impl SimpleExpr {
    pub fn typ(&self) -> Type {
        match self {
            Self::Uninitialized => Type::Unknown,
            Self::Boolean(_) => Type::Bit,
            Self::Int(_) => Type::Int,
            Self::String(_) => Type::String,
            Self::Code(_) => Type::Code,
            Self::Bits(bits) => Type::Bits(bits.len()),
            Self::List(_, typ) => Type::List(Box::new(typ.clone())),
            Self::Identifier(_, Some((_, typ))) => typ.clone(),
            Self::Identifier(_, None) => Type::Unknown,
            Self::BangOperator(_, _) => Type::Unknown, // TODO
        }
    }
}

impl std::fmt::Display for SimpleExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Uninitialized => write!(f, "?"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{value}\""),
            Self::Code(value) => write!(f, "[{{ {value} }}]"),
            Self::Bits(values) => {
                write!(
                    f,
                    "{{ {} }}",
                    values
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::List(values, _) => {
                write!(
                    f,
                    "[ {} ]",
                    values
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Identifier(name, _) => write!(f, "{name}"),
            Self::BangOperator(op, args) => write!(
                f,
                "{}({})",
                op,
                args.iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DagArg {
    value: Expr,
    var_name: EcoString,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BangOperatorOp {
    XConcat,
    XAdd,
    XSub,
    XMul,
    XDiv,
    XNot,
    XLog2,
    XAnd,
    XOr,
    XXor,
    XSra,
    XSrl,
    XShl,
    XListConcat,
    XListSplat,
    XStrConcat,
    XInterleave,
    XSubstr,
    XFind,
    XCast,
    XSubst,
    XForEach,
    XFilter,
    XFoldl,
    XHead,
    XTail,
    XSize,
    XEmpty,
    XIf,
    XCond,
    XEq,
    XIsA,
    XDag,
    XNe,
    XLe,
    XLt,
    XGe,
    XGt,
    XSetDagOp,
    XGetDagOp,
    XExists,
    XListRemove,
    XToLower,
    XToUpper,
    XRange,
    XGetDagArg,
    XGetDagName,
    XSetDagArg,
    XSetDagName,
}

impl From<SyntaxKind> for BangOperatorOp {
    fn from(value: SyntaxKind) -> Self {
        match value {
            SyntaxKind::XConcat => Self::XConcat,
            SyntaxKind::XAdd => Self::XAdd,
            SyntaxKind::XSub => Self::XSub,
            SyntaxKind::XMul => Self::XMul,
            SyntaxKind::XDiv => Self::XDiv,
            SyntaxKind::XNot => Self::XNot,
            SyntaxKind::XLog2 => Self::XLog2,
            SyntaxKind::XAnd => Self::XAnd,
            SyntaxKind::XOr => Self::XOr,
            SyntaxKind::XXor => Self::XXor,
            SyntaxKind::XSra => Self::XSra,
            SyntaxKind::XSrl => Self::XSrl,
            SyntaxKind::XShl => Self::XShl,
            SyntaxKind::XListConcat => Self::XListConcat,
            SyntaxKind::XListSplat => Self::XListSplat,
            SyntaxKind::XStrConcat => Self::XStrConcat,
            SyntaxKind::XInterleave => Self::XInterleave,
            SyntaxKind::XSubstr => Self::XSubstr,
            SyntaxKind::XFind => Self::XFind,
            SyntaxKind::XCast => Self::XCast,
            SyntaxKind::XSubst => Self::XSubst,
            SyntaxKind::XForEach => Self::XForEach,
            SyntaxKind::XFilter => Self::XFilter,
            SyntaxKind::XFoldl => Self::XFoldl,
            SyntaxKind::XHead => Self::XHead,
            SyntaxKind::XTail => Self::XTail,
            SyntaxKind::XSize => Self::XSize,
            SyntaxKind::XEmpty => Self::XEmpty,
            SyntaxKind::XIf => Self::XIf,
            SyntaxKind::XCond => Self::XCond,
            SyntaxKind::XEq => Self::XEq,
            SyntaxKind::XIsA => Self::XIsA,
            SyntaxKind::XDag => Self::XDag,
            SyntaxKind::XNe => Self::XNe,
            SyntaxKind::XLe => Self::XLe,
            SyntaxKind::XLt => Self::XLt,
            SyntaxKind::XGe => Self::XGe,
            SyntaxKind::XGt => Self::XGt,
            SyntaxKind::XSetDagOp => Self::XSetDagOp,
            SyntaxKind::XGetDagOp => Self::XGetDagOp,
            SyntaxKind::XExists => Self::XExists,
            SyntaxKind::XListRemove => Self::XListRemove,
            SyntaxKind::XToLower => Self::XToLower,
            SyntaxKind::XToUpper => Self::XToUpper,
            SyntaxKind::XRange => Self::XRange,
            SyntaxKind::XGetDagArg => Self::XGetDagArg,
            SyntaxKind::XGetDagName => Self::XGetDagName,
            SyntaxKind::XSetDagArg => Self::XSetDagArg,
            SyntaxKind::XSetDagName => Self::XSetDagName,
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for BangOperatorOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::XConcat => write!(f, "!concat"),
            Self::XAdd => write!(f, "!add"),
            Self::XSub => write!(f, "!sub"),
            Self::XMul => write!(f, "!mul"),
            Self::XDiv => write!(f, "!div"),
            Self::XNot => write!(f, "!not"),
            Self::XLog2 => write!(f, "!log2"),
            Self::XAnd => write!(f, "!and"),
            Self::XOr => write!(f, "!or"),
            Self::XXor => write!(f, "!xor"),
            Self::XSra => write!(f, "!sra"),
            Self::XSrl => write!(f, "!srl"),
            Self::XShl => write!(f, "!shl"),
            Self::XListConcat => write!(f, "!listconcat"),
            Self::XListSplat => write!(f, "!listsplat"),
            Self::XStrConcat => write!(f, "!strconcat"),
            Self::XInterleave => write!(f, "!interleave"),
            Self::XSubstr => write!(f, "!substr"),
            Self::XFind => write!(f, "!find"),
            Self::XCast => write!(f, "!cast"),
            Self::XSubst => write!(f, "!subst"),
            Self::XForEach => write!(f, "!foreach"),
            Self::XFilter => write!(f, "!filter"),
            Self::XFoldl => write!(f, "!foldl"),
            Self::XHead => write!(f, "!head"),
            Self::XTail => write!(f, "!tail"),
            Self::XSize => write!(f, "!size"),
            Self::XEmpty => write!(f, "!empty"),
            Self::XIf => write!(f, "!if"),
            Self::XCond => write!(f, "!cond"),
            Self::XEq => write!(f, "!eq"),
            Self::XIsA => write!(f, "!isa"),
            Self::XDag => write!(f, "!dag"),
            Self::XNe => write!(f, "!ne"),
            Self::XLe => write!(f, "!le"),
            Self::XLt => write!(f, "!lt"),
            Self::XGe => write!(f, "!ge"),
            Self::XGt => write!(f, "!gt"),
            Self::XSetDagOp => write!(f, "!setdagop"),
            Self::XGetDagOp => write!(f, "!getdagop"),
            Self::XExists => write!(f, "!exists"),
            Self::XListRemove => write!(f, "!listremove"),
            Self::XToLower => write!(f, "!tolower"),
            Self::XToUpper => write!(f, "!toupper"),
            Self::XRange => write!(f, "!range"),
            Self::XGetDagArg => write!(f, "!getdagarg"),
            Self::XGetDagName => write!(f, "!getdagname"),
            Self::XSetDagArg => write!(f, "!setdagarg"),
            Self::XSetDagName => write!(f, "!setdagname"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Uninitialized,
    Int(i64),
    String(String),
    Bits(Vec<Value>),
    List(Vec<Value>, Type),
}

impl Value {
    pub fn typ(&self) -> Type {
        match self {
            Self::Uninitialized => Type::Unknown,
            Self::Int(_) => Type::Int,
            Self::String(_) => Type::String,
            Self::Bits(bits) => Type::Bits(bits.len()),
            Self::List(_, typ) => Type::List(Box::new(typ.clone())),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Uninitialized => write!(f, "?"),
            Self::Int(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{value}\""),
            Self::Bits(values) => {
                write!(
                    f,
                    "{{ {} }}",
                    values
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::List(values, _) => {
                write!(
                    f,
                    "[ {} ]",
                    values
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum SymbolId {
    ClassId(ClassId),
    TemplateArgumentId(TemplateArgumentId),
    FieldId(FieldId),
    DefId(DefId),
}

impl SymbolId {
    pub fn as_class_id(&self) -> Option<ClassId> {
        match self {
            SymbolId::ClassId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_template_argument_id(&self) -> Option<TemplateArgumentId> {
        match self {
            SymbolId::TemplateArgumentId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_field_id(&self) -> Option<FieldId> {
        match self {
            SymbolId::FieldId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_def_id(&self) -> Option<DefId> {
        match self {
            SymbolId::DefId(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a> {
    Class(&'a Class),
    TemplateArgument(&'a TemplateArgument),
    Field(&'a Field),
    Def(&'a Def),
}

impl<'a> Symbol<'a> {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Class(class) => &class.name,
            Self::TemplateArgument(template_arg) => &template_arg.name,
            Self::Field(field) => &field.name,
            Self::Def(def) => &def.name,
        }
    }

    pub fn define_loc(&self) -> &FileRange {
        match self {
            Self::Class(class) => &class.define_loc,
            Self::TemplateArgument(template_arg) => &template_arg.define_loc,
            Self::Field(field) => &field.define_loc,
            Self::Def(def) => &def.define_loc,
        }
    }

    pub fn reference_locs(&self) -> &[FileRange] {
        match self {
            Self::Class(class) => &class.reference_locs,
            Self::TemplateArgument(template_arg) => &template_arg.reference_locs,
            Self::Field(field) => &field.reference_locs,
            Self::Def(def) => &def.reference_locs,
        }
    }

    pub fn as_class(&self) -> Option<&Class> {
        match self {
            Self::Class(class) => Some(class),
            _ => None,
        }
    }

    pub fn as_template_argument(&self) -> Option<&TemplateArgument> {
        match self {
            Self::TemplateArgument(template_arg) => Some(template_arg),
            _ => None,
        }
    }

    pub fn as_field(&self) -> Option<&Field> {
        match self {
            Self::Field(field) => Some(field),
            _ => None,
        }
    }

    pub fn as_def(&self) -> Option<&Def> {
        match self {
            Self::Def(def) => Some(def),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolMut<'a> {
    Class(&'a mut Class),
    TemplateArgument(&'a mut TemplateArgument),
    Field(&'a mut Field),
    Def(&'a mut Def),
}

impl<'a> SymbolMut<'a> {
    pub fn add_reference(&mut self, reference_loc: FileRange) {
        match self {
            Self::Class(class) => class.reference_locs.push(reference_loc),
            Self::TemplateArgument(template_arg) => template_arg.reference_locs.push(reference_loc),
            Self::Field(field) => field.reference_locs.push(reference_loc),
            Self::Def(def) => def.reference_locs.push(reference_loc),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit,
    Int,
    String,
    Dag,
    Bits(usize),
    List(Box<Type>),
    Class(ClassId, EcoString),
    Code,
    Unknown, // for uninitialized
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bit => write!(f, "bit"),
            Self::Int => write!(f, "int"),
            Self::String => write!(f, "string"),
            Self::Dag => write!(f, "dag"),
            Self::Bits(width) => write!(f, "bits<{}>", width),
            Self::List(typ) => write!(f, "list<{}>", typ),
            Self::Class(_, name) => write!(f, "{}", name),
            Self::Code => write!(f, "code"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    template_arg_list: Arena<TemplateArgument>,
    field_list: Arena<Field>,
    def_list: Arena<Def>,
    file_to_symbol_list: HashMap<FileId, Vec<SymbolId>>,
    pos_to_symbol_map: HashMap<FileId, IntervalMap<TextSize, SymbolId>>,
}

// immutable api
impl SymbolMap {
    pub fn class(&self, class_id: ClassId) -> &Class {
        self.class_list.get(class_id).expect("invalid class id")
    }

    pub fn class_mut(&mut self, class_id: ClassId) -> &mut Class {
        self.class_list.get_mut(class_id).expect("invalid class id")
    }

    pub fn template_arg(&self, template_arg_id: TemplateArgumentId) -> &TemplateArgument {
        self.template_arg_list
            .get(template_arg_id)
            .expect("invalid template argument id")
    }

    pub fn template_arg_mut(
        &mut self,
        template_arg_id: TemplateArgumentId,
    ) -> &mut TemplateArgument {
        self.template_arg_list
            .get_mut(template_arg_id)
            .expect("invalid template argument id")
    }

    pub fn field(&self, field_id: FieldId) -> &Field {
        self.field_list.get(field_id).expect("invalid field id")
    }

    pub fn field_mut(&mut self, field_id: FieldId) -> &mut Field {
        self.field_list.get_mut(field_id).expect("invalid field id")
    }

    pub fn def(&self, def_id: DefId) -> &Def {
        self.def_list.get(def_id).expect("invalid def id")
    }

    pub fn def_mut(&mut self, def_id: DefId) -> &mut Def {
        self.def_list.get_mut(def_id).expect("invalid def id")
    }

    pub fn symbol(&self, id: SymbolId) -> Symbol {
        match id {
            SymbolId::ClassId(class_id) => Symbol::Class(self.class(class_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                Symbol::TemplateArgument(self.template_arg(template_arg_id))
            }
            SymbolId::FieldId(field_id) => Symbol::Field(self.field(field_id)),
            SymbolId::DefId(def_id) => Symbol::Def(self.def(def_id)),
        }
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> SymbolMut {
        match id {
            SymbolId::ClassId(class_id) => SymbolMut::Class(self.class_mut(class_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                SymbolMut::TemplateArgument(self.template_arg_mut(template_arg_id))
            }
            SymbolId::FieldId(field_id) => SymbolMut::Field(self.field_mut(field_id)),
            SymbolId::DefId(def_id) => SymbolMut::Def(self.def_mut(def_id)),
        }
    }

    pub fn iter_symbol_in(&self, file_id: FileId) -> Option<impl Iterator<Item = SymbolId>> {
        self.file_to_symbol_list
            .get(&file_id)
            .cloned()
            .map(|class_list| class_list.into_iter())
    }

    pub fn find_symbol_at(&self, pos: FilePosition) -> Option<Symbol> {
        let id = self
            .pos_to_symbol_map
            .get(&pos.file)
            .and_then(|map| map.values_overlap(pos.position).next().cloned())?;
        Some(self.symbol(id))
    }
}

// mutable api
impl SymbolMap {
    pub fn add_class(&mut self, class: Class) -> ClassId {
        let define_loc = class.define_loc;
        let id = self.class_list.alloc(class);
        self.file_to_symbol_list
            .entry(define_loc.file)
            .or_default()
            .push(id.into());
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), id.into());
        id
    }

    pub fn add_template_argument(&mut self, template_arg: TemplateArgument) -> TemplateArgumentId {
        let define_loc = template_arg.define_loc;
        let id = self.template_arg_list.alloc(template_arg);
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), SymbolId::TemplateArgumentId(id));
        id
    }

    pub fn add_field(&mut self, field: Field) -> FieldId {
        let define_loc = field.define_loc;
        let id = self.field_list.alloc(field);
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), SymbolId::FieldId(id));
        id
    }

    pub fn add_def(&mut self, def: Def) -> DefId {
        let define_loc = def.define_loc;
        let id = self.def_list.alloc(def);
        self.file_to_symbol_list
            .entry(define_loc.file)
            .or_default()
            .push(id.into());
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), id.into());
        id
    }

    pub fn add_reference(&mut self, symbol_id: impl Into<SymbolId>, reference_loc: FileRange) {
        let symbol_id = symbol_id.into();
        let mut symbol = self.symbol_mut(symbol_id);
        symbol.add_reference(reference_loc);
        self.pos_to_symbol_map
            .entry(reference_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(reference_loc.range.into(), symbol_id);
    }
}

// FIXME
// because iset::IntervalMap does not implement PartialEq and Eq, we have to implement them
#[derive(Debug, Default)]
struct IntervalMap<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq>(
    iset::IntervalMap<T, V>,
);

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> IntervalMap<T, V> {
    fn new() -> Self {
        Self(iset::IntervalMap::new())
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Deref for IntervalMap<T, V> {
    type Target = iset::IntervalMap<T, V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> DerefMut for IntervalMap<T, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> PartialEq for IntervalMap<T, V> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        for ((range1, value1), (range2, value2)) in self.0.iter(..).zip(other.0.iter(..)) {
            if range1 != range2 || value1 != value2 {
                return false;
            }
        }
        true
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Eq for IntervalMap<T, V> {}
