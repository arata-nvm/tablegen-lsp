use ecow::EcoString;

use super::{record::RecordId, record_field::RecordFieldId, SymbolMap};

// TODO: Eq and isa may cause confusion
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit,
    Int,
    String,
    Code,
    Dag,
    Bits(usize),
    List(Box<Type>),
    Record(RecordId, EcoString),
    Uninitialized,
    Unknown,
}

impl Type {
    pub fn element_typ(&self) -> Option<Type> {
        match self {
            Self::Bits(_) => Some(Self::Bit),
            Self::List(elm_typ) => Some(*elm_typ.clone()),
            _ => None,
        }
    }

    pub fn find_field(&self, symbol_map: &SymbolMap, name: &EcoString) -> Option<RecordFieldId> {
        let Self::Record(record_id, _) = self else {
            return None;
        };

        let record = symbol_map.record(*record_id);
        record.find_field(symbol_map, name)
    }

    pub fn isa(&self, symbol_map: &SymbolMap, other: &Type) -> bool {
        match (self, other) {
            // 指定されたビット幅でIntを表現できない場合はエラーを出す必要がある
            (Self::Int, Self::Bits(_)) | (Self::Bits(_), Self::Int) => true,
            (Self::String, Self::Code) | (Self::Code, Self::String) => true,
            (Self::List(self_elm_typ), Self::List(other_elm_typ)) => {
                self_elm_typ.isa(symbol_map, other_elm_typ)
            }
            (Self::Record(self_record_id, _), Self::Record(other_record_id, _)) => {
                if self_record_id == other_record_id {
                    return true;
                }
                let self_record = symbol_map.record(*self_record_id);
                self_record.is_subclass_of(symbol_map, *other_record_id)
            }
            _ => self == other,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bit => write!(f, "bit"),
            Self::Int => write!(f, "int"),
            Self::String => write!(f, "string"),
            Self::Code => write!(f, "code"),
            Self::Dag => write!(f, "dag"),
            Self::Bits(width) => write!(f, "bits<{}>", width),
            Self::List(typ) => write!(f, "list<{}>", typ),
            Self::Record(_, name) => write!(f, "{}", name),
            Self::Uninitialized => write!(f, "uninitialized"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
