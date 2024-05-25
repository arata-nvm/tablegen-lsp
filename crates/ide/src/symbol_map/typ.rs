use ecow::EcoString;

use super::{class::ClassId, def::DefId};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit,
    Int,
    String,
    Dag,
    Bits(usize),
    List(Box<Type>),
    Class(ClassId, EcoString),
    Def(DefId, EcoString), // TODO
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
            Self::Def(_, name) => write!(f, "{}(def)", name),
            Self::Code => write!(f, "code"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
