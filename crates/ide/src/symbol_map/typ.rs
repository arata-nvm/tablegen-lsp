use ecow::EcoString;

use super::{
    SymbolMap,
    record::{RecordFieldId, RecordId},
};

#[macro_export]
macro_rules! TY {
    [bit] => {$crate::symbol_map::typ::Type::Bit};
    [int] => {$crate::symbol_map::typ::Type::Int};
    [string] => {$crate::symbol_map::typ::Type::String};
    [code] => {$crate::symbol_map::typ::Type::Code};
    [dag] => {$crate::symbol_map::typ::Type::Dag};
    [bits<$len:tt>] => {$crate::symbol_map::typ::Type::Bits($len)};
    [list<$elm_typ:tt>] => {$crate::symbol_map::typ::Type::List(Box::new(TY!($elm_typ)))};
    [?] => {$crate::symbol_map::typ::Type::Uninitialized};
}

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
    NotResolved(EcoString),
    Uninitialized,
    Unknown,
    Any, // for empty list
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

    pub fn can_be_casted_to(&self, symbol_map: &SymbolMap, other: &Type) -> bool {
        match (self, other) {
            (Self::Uninitialized, _) | (_, Self::Uninitialized) => true,
            (Self::Any, _) | (_, Self::Any) => true,
            // 0,1以外の値の場合はエラーを出す必要がある
            (Self::Int, Self::Bit) | (Self::Bit, Self::Int) => true,
            // 指定されたビット幅でIntを表現できない場合はエラーを出す必要がある
            (Self::Int, Self::Bits(_)) | (Self::Bits(_), Self::Int) => true,
            (Self::String, Self::Code) | (Self::Code, Self::String) => true,
            (Self::List(self_elm_typ), Self::List(other_elm_typ)) => {
                self_elm_typ.can_be_casted_to(symbol_map, other_elm_typ)
            }
            (Self::Record(self_record_id, _), Self::Record(other_record_id, _)) => {
                if self_record_id == other_record_id {
                    return true;
                }
                match other_record_id {
                    // class, class || def, class
                    RecordId::Class(other_class_id) => {
                        let self_record = symbol_map.record(*self_record_id);
                        self_record.is_subclass_of(symbol_map, *other_class_id)
                    }
                    // class, def || def, def
                    RecordId::Def(_) => false,
                }
            }
            _ => self == other,
        }
    }

    pub fn is_bits(&self) -> bool {
        matches!(self, Self::Bits(_) | Self::Uninitialized)
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(_) | Self::Uninitialized)
    }

    pub fn is_record(&self) -> bool {
        matches!(self, Self::Record(_, _) | Self::Uninitialized)
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
            Self::Bits(width) => write!(f, "bits<{width}>"),
            Self::List(typ) => write!(f, "list<{typ}>"),
            Self::Record(_, name) => write!(f, "{name}"),
            Self::NotResolved(name) => write!(f, "{name}"),
            Self::Uninitialized => write!(f, "uninitialized"),
            Self::Unknown => write!(f, "unknown"),
            Self::Any => write!(f, "any"),
        }
    }
}
