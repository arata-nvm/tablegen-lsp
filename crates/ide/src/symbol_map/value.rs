use ecow::EcoString;

use super::typ::Type;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Uninitialized,
    Int(i64),
    String(EcoString),
    Bits(Vec<Value>),
    List(Vec<Value>, Type),
    Dag(Box<DagArgValue>, Vec<DagArgValue>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DagArgValue {
    pub value: Value,
    pub var_name: Option<EcoString>,
}

impl std::fmt::Display for DagArgValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.var_name {
            Some(ref var_name) => write!(f, "{}:${}", self.value, var_name),
            None => write!(f, "{}", self.value),
        }
    }
}

impl Value {
    pub fn typ(&self) -> Type {
        match self {
            Self::Uninitialized => Type::Unknown,
            Self::Int(_) => Type::Int,
            Self::String(_) => Type::String,
            Self::Bits(bits) => Type::Bits(bits.len()),
            Self::List(_, typ) => Type::List(Box::new(typ.clone())),
            Self::Dag(_, _) => Type::Dag,
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
            Self::Dag(op, args) => write!(
                f,
                "({} {})",
                op,
                args.iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}
