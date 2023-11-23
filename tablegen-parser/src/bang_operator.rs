use std::fmt;

use crate::syntax_kind::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BangOperator {
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

impl BangOperator {
    pub fn min_num_of_args(&self) -> usize {
        use self::BangOperator::*;
        match self {
            XRange => 1,
            XFind | XSubstr => 2,
            XCast | XCond | XEmpty | XExists | XGetDagOp | XHead | XIsA | XLog2 | XNot | XSize
            | XTail | XToLower | XToUpper | XXor => 1,
            XAdd | XAnd | XConcat | XDiv | XEq | XGe | XGetDagArg | XGetDagName | XGt
            | XInterleave | XLe | XListConcat | XListRemove | XListSplat | XLt | XMul | XNe
            | XOr | XSetDagOp | XShl | XSra | XSrl | XStrConcat | XSub => 2,
            XDag | XFilter | XForEach | XIf | XSetDagArg | XSetDagName | XSubst => 3,
            XFoldl => 5,
        }
    }

    pub fn max_num_of_args(&self) -> usize {
        use self::BangOperator::*;
        match self {
            XRange => 3,
            XFind | XSubstr => 3,
            XAdd | XAnd | XConcat | XCond | XListConcat | XMul | XOr | XStrConcat | XXor => {
                usize::MAX
            }
            _ => self.min_num_of_args(),
        }
    }

    pub fn is_valid_num_of_args(&self, num_of_args: usize) -> bool {
        self.min_num_of_args() <= num_of_args && num_of_args <= self.max_num_of_args()
    }
}

impl fmt::Display for BangOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::BangOperator::*;
        match self {
            XConcat => write!(f, "!concat"),
            XAdd => write!(f, "!add"),
            XSub => write!(f, "!sub"),
            XMul => write!(f, "!mul"),
            XDiv => write!(f, "!div"),
            XNot => write!(f, "!not"),
            XLog2 => write!(f, "!log2"),
            XAnd => write!(f, "!and"),
            XOr => write!(f, "!or"),
            XXor => write!(f, "!xor"),
            XSra => write!(f, "!sra"),
            XSrl => write!(f, "!srl"),
            XShl => write!(f, "!shl"),
            XListConcat => write!(f, "!listconcat"),
            XListSplat => write!(f, "!listsplat"),
            XStrConcat => write!(f, "!strconcat"),
            XInterleave => write!(f, "!interleave"),
            XSubstr => write!(f, "!substr"),
            XFind => write!(f, "!find"),
            XCast => write!(f, "!cast"),
            XSubst => write!(f, "!subst"),
            XForEach => write!(f, "!foreach"),
            XFilter => write!(f, "!filter"),
            XFoldl => write!(f, "!foldl"),
            XHead => write!(f, "!head"),
            XTail => write!(f, "!tail"),
            XSize => write!(f, "!size"),
            XEmpty => write!(f, "!empty"),
            XIf => write!(f, "!if"),
            XCond => write!(f, "!cond"),
            XEq => write!(f, "!eq"),
            XIsA => write!(f, "!isa"),
            XDag => write!(f, "!dag"),
            XNe => write!(f, "!ne"),
            XLe => write!(f, "!le"),
            XLt => write!(f, "!lt"),
            XGe => write!(f, "!ge"),
            XGt => write!(f, "!gt"),
            XSetDagOp => write!(f, "!setdagop"),
            XGetDagOp => write!(f, "!getdagop"),
            XExists => write!(f, "!exists"),
            XListRemove => write!(f, "!listremove"),
            XToLower => write!(f, "!tolower"),
            XToUpper => write!(f, "!toupper"),
            XRange => write!(f, "!range"),
            XGetDagArg => write!(f, "!getdagarg"),
            XGetDagName => write!(f, "!getdagname"),
            XSetDagArg => write!(f, "!setdagarg"),
            XSetDagName => write!(f, "!setdagname"),
        }
    }
}

impl TryFrom<SyntaxKind> for BangOperator {
    type Error = ();

    fn try_from(kind: SyntaxKind) -> Result<Self, Self::Error> {
        match kind {
            SyntaxKind::XConcat => Ok(Self::XConcat),
            SyntaxKind::XAdd => Ok(Self::XAdd),
            SyntaxKind::XSub => Ok(Self::XSub),
            SyntaxKind::XMul => Ok(Self::XMul),
            SyntaxKind::XDiv => Ok(Self::XDiv),
            SyntaxKind::XNot => Ok(Self::XNot),
            SyntaxKind::XLog2 => Ok(Self::XLog2),
            SyntaxKind::XAnd => Ok(Self::XAnd),
            SyntaxKind::XOr => Ok(Self::XOr),
            SyntaxKind::XXor => Ok(Self::XXor),
            SyntaxKind::XSra => Ok(Self::XSra),
            SyntaxKind::XSrl => Ok(Self::XSrl),
            SyntaxKind::XShl => Ok(Self::XShl),
            SyntaxKind::XListConcat => Ok(Self::XListConcat),
            SyntaxKind::XListSplat => Ok(Self::XListSplat),
            SyntaxKind::XStrConcat => Ok(Self::XStrConcat),
            SyntaxKind::XInterleave => Ok(Self::XInterleave),
            SyntaxKind::XSubstr => Ok(Self::XSubstr),
            SyntaxKind::XFind => Ok(Self::XFind),
            SyntaxKind::XCast => Ok(Self::XCast),
            SyntaxKind::XSubst => Ok(Self::XSubst),
            SyntaxKind::XForEach => Ok(Self::XForEach),
            SyntaxKind::XFilter => Ok(Self::XFilter),
            SyntaxKind::XFoldl => Ok(Self::XFoldl),
            SyntaxKind::XHead => Ok(Self::XHead),
            SyntaxKind::XTail => Ok(Self::XTail),
            SyntaxKind::XSize => Ok(Self::XSize),
            SyntaxKind::XEmpty => Ok(Self::XEmpty),
            SyntaxKind::XIf => Ok(Self::XIf),
            SyntaxKind::XCond => Ok(Self::XCond),
            SyntaxKind::XEq => Ok(Self::XEq),
            SyntaxKind::XIsA => Ok(Self::XIsA),
            SyntaxKind::XDag => Ok(Self::XDag),
            SyntaxKind::XNe => Ok(Self::XNe),
            SyntaxKind::XLe => Ok(Self::XLe),
            SyntaxKind::XLt => Ok(Self::XLt),
            SyntaxKind::XGe => Ok(Self::XGe),
            SyntaxKind::XGt => Ok(Self::XGt),
            SyntaxKind::XSetDagOp => Ok(Self::XSetDagOp),
            SyntaxKind::XGetDagOp => Ok(Self::XGetDagOp),
            SyntaxKind::XExists => Ok(Self::XExists),
            SyntaxKind::XListRemove => Ok(Self::XListRemove),
            SyntaxKind::XToLower => Ok(Self::XToLower),
            SyntaxKind::XToUpper => Ok(Self::XToUpper),
            SyntaxKind::XRange => Ok(Self::XRange),
            SyntaxKind::XGetDagArg => Ok(Self::XGetDagArg),
            SyntaxKind::XGetDagName => Ok(Self::XGetDagName),
            SyntaxKind::XSetDagArg => Ok(Self::XSetDagArg),
            SyntaxKind::XSetDagName => Ok(Self::XSetDagName),
            _ => Err(()),
        }
    }
}
