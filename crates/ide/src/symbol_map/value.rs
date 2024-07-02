use ecow::EcoString;

use super::{def::DefId, typ::Type, SymbolMap};

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub enum Value {
    #[default]
    Uninitialized,
    Bit(bool),
    Int(i64),
    String(EcoString),
    Bits(i64, usize),
    List(Vec<Value>, Type),
    Dag(Box<DagArgValue>, Vec<DagArgValue>),
    DefIdentifier(EcoString, DefId, Type),
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
            Self::Bit(_) => Type::Bit,
            Self::Int(_) => Type::Int,
            Self::String(_) => Type::String,
            Self::Bits(_, len) => Type::Bits(*len),
            Self::List(_, typ) => Type::List(Box::new(typ.clone())),
            Self::Dag(_, _) => Type::Dag,
            Self::DefIdentifier(_, _, typ) => typ.clone(),
        }
    }

    pub fn cast_to(&self, symbol_map: &SymbolMap, typ: &Type) -> Option<Value> {
        if self.typ().isa(symbol_map, typ) {
            return Some(self.clone());
        }

        match (self, typ) {
            (Value::Bit(false), Type::Int) => Some(Value::Int(0)),
            (Value::Bit(true), Type::Int) => Some(Value::Int(1)),
            (Value::Bits(0, _), Type::Bit) => Some(Value::Bit(false)),
            (Value::Bits(_, _), Type::Bit) => Some(Value::Bit(true)),
            (Value::Int(0), Type::Bit) => Some(Value::Bit(false)),
            (Value::Int(_), Type::Bit) => Some(Value::Bit(true)),
            (Value::Int(int), Type::Bits(len)) => Some(Value::Bits(*int, *len)),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Uninitialized => write!(f, "?"),
            Self::Bit(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{value}\""),
            Self::Bits(value, len) => {
                write!(f, "{{ ")?;
                let bits = (0..*len)
                    .map(|i| (value >> (len - i - 1)) & 1)
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{bits}")?;
                write!(f, " }}")
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
            Self::DefIdentifier(name, _, _) => write!(f, "{}", name),
        }
    }
}
