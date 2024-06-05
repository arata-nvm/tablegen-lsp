use std::collections::HashMap;

use ecow::EcoString;
use syntax::syntax_kind::SyntaxKind;

use super::{class::ClassId, symbol::SymbolId, template_arg::TemplateArgumentId, typ::Type};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Simple(SimpleExpr),
    // RangeSuffix(Box<Expr>, Vec<Range>),
    // SliceSuffix(Box<Expr>, Vec<Slice>),
    FieldSuffix(Box<Expr>, EcoString, Type),
    // Paste(Box<Expr>, SimpleExpr),
}

pub type Replacement = HashMap<TemplateArgumentId, Expr>;

impl Expr {
    pub fn replaced(self, replacement: &Replacement) -> Self {
        match self {
            Self::Simple(SimpleExpr::Identifier(_, SymbolId::TemplateArgumentId(id), _)) => {
                match replacement.get(&id) {
                    Some(expr) => expr.clone(),
                    None => self,
                }
            }
            Self::Simple(SimpleExpr::Bits(bits)) => Self::Simple(SimpleExpr::Bits(
                bits.into_iter()
                    .map(|bit| bit.replaced(replacement))
                    .collect(),
            )),
            Self::Simple(SimpleExpr::List(values, typ)) => Self::Simple(SimpleExpr::List(
                values
                    .into_iter()
                    .map(|value| value.replaced(replacement))
                    .collect(),
                typ.clone(),
            )),
            Self::Simple(SimpleExpr::Dag(op, args)) => Self::Simple(SimpleExpr::Dag(
                Box::new(op.replaced(replacement)),
                args.into_iter()
                    .map(|arg| arg.replaced(replacement))
                    .collect(),
            )),
            Self::Simple(SimpleExpr::ClassValue(name, class_id, args)) => {
                Self::Simple(SimpleExpr::ClassValue(
                    name,
                    class_id,
                    args.into_iter()
                        .map(|arg| arg.replaced(replacement))
                        .collect(),
                ))
            }
            Self::Simple(SimpleExpr::BangOperator(op, args)) => {
                Self::Simple(SimpleExpr::BangOperator(
                    op,
                    args.into_iter()
                        .map(|arg| arg.replaced(replacement))
                        .collect(),
                ))
            }
            Self::Simple(SimpleExpr::CondOperator(clauses)) => {
                Self::Simple(SimpleExpr::CondOperator(
                    clauses
                        .into_iter()
                        .map(|clause| clause.replaced(replacement))
                        .collect(),
                ))
            }
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
    String(EcoString),
    Code(EcoString),
    Bits(Vec<Expr>),
    List(Vec<Expr>, Type),
    Dag(Box<DagArg>, Vec<DagArg>),
    Identifier(EcoString, SymbolId, Type),
    ClassValue(EcoString, ClassId, Vec<Expr>),
    BangOperator(BangOperatorOp, Vec<Expr>),
    CondOperator(Vec<CondClause>),
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
            Self::Dag(_, _) => Type::Dag,
            Self::Identifier(_, _, typ) => typ.clone(),
            Self::ClassValue(class_name, class_id, _) => Type::Class(*class_id, class_name.clone()),
            Self::BangOperator(_, _) => Type::Unknown, // TODO
            Self::CondOperator(_) => Type::Unknown,    // TODO
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
            Self::Dag(op, args) => write!(
                f,
                "({} {})",
                op,
                args.iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Identifier(name, _, _) => write!(f, "{name}"),
            Self::ClassValue(name, _, args) => write!(
                f,
                "{}<{}>",
                name,
                args.iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::BangOperator(op, args) => write!(
                f,
                "{}({})",
                op,
                args.iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::CondOperator(clause_list) => write!(
                f,
                "!cond({})",
                clause_list
                    .iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DagArg {
    pub value: Expr,
    pub var_name: Option<EcoString>,
}

impl DagArg {
    pub fn replaced(self, replacement: &Replacement) -> Self {
        Self {
            value: self.value.replaced(replacement),
            var_name: self.var_name.clone(),
        }
    }
}

impl std::fmt::Display for DagArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.var_name {
            Some(ref var_name) => write!(f, "{}:${}", self.value, var_name),
            None => write!(f, "{}", self.value),
        }
    }
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
pub struct CondClause {
    pub condition: Expr,
    pub value: Expr,
}

impl CondClause {
    pub fn replaced(self, replacement: &Replacement) -> Self {
        Self {
            condition: self.condition.replaced(replacement),
            value: self.value.replaced(replacement),
        }
    }
}

impl std::fmt::Display for CondClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.condition, self.value)
    }
}
