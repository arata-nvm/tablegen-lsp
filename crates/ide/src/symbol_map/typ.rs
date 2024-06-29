use ecow::EcoString;

use super::{class::ClassId, def::DefId, SymbolMap};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit,
    Int,
    String,
    Dag,
    Bits(usize),
    List(Box<Type>),
    Class(ClassId, EcoString),
    Def(DefId, EcoString),
    Code,
    Unknown, // for uninitialized
}

impl Type {
    pub fn isa(&self, symbol_map: &SymbolMap, other: &Type) -> bool {
        if self == other {
            return true;
        }

        match (self, other) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Class(class_id1, _), Type::Class(class_id2, _)) => {
                let class1 = symbol_map.class(*class_id1);
                class1.parent_class_list.contains(class_id2)
            }
            (Type::Def(def_id, _), Type::Class(class_id, _)) => {
                let def = symbol_map.def(*def_id);
                def.parent_class_list.contains(class_id)
            }
            _ => false,
        }
    }
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
            Self::Def(_, name) => write!(f, "{}", name),
            Self::Code => write!(f, "code"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
