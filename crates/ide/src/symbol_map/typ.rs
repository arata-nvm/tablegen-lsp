use ecow::EcoString;

use super::record::RecordId;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit,
    Int,
    String,
    Dag,
    Bits(usize),
    List(Box<Type>),
    Record(RecordId, EcoString),
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
            Self::Record(_, name) => write!(f, "{}", name),
            Self::Code => write!(f, "code"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
