use std::collections::HashSet;

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
    Uninitialized,
    Unknown,                 // for error recovery
    NamedUnknown(EcoString), // for error recovery
    Any,                     // for empty list
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("type '{0}' is not bits type")]
    IsNotBitsType(Type),
    #[error("cannot set bit index {0} for bits<{1}>")]
    CannotSetBitIndex(usize, usize),
    #[error("duplicate bit index {0}")]
    DuplicateBitIndex(usize),
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
            (Self::Uninitialized | Self::Unknown | Self::NamedUnknown(_) | Self::Any, _)
            | (_, Self::Uninitialized | Self::Unknown | Self::NamedUnknown(_) | Self::Any) => true,
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
                match (self_record_id, other_record_id) {
                    (_, RecordId::Class(other_class_id)) => {
                        let self_record = symbol_map.record(*self_record_id);
                        self_record.is_subclass_of(symbol_map, *other_class_id)
                    }
                    (RecordId::Class(_), RecordId::Def(_)) => false,
                    (RecordId::Def(self_def_id), RecordId::Def(other_def_id)) => {
                        let self_def = symbol_map.def(*self_def_id);
                        let other_def = symbol_map.def(*other_def_id);
                        self_def.parent_list.iter().all(|self_parent_id| {
                            let self_parent = symbol_map.class(*self_parent_id);
                            other_def.parent_list.iter().any(|other_parent_id| {
                                self_parent_id == other_parent_id
                                    || self_parent.is_subclass_of(symbol_map, *other_parent_id)
                            })
                        })
                    }
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

    pub fn with_bits(&self, bits: Vec<usize>) -> Result<Self, TypeError> {
        let Self::Bits(old_width) = self else {
            return Err(TypeError::IsNotBitsType(self.clone()));
        };

        let mut used = HashSet::new();
        for bit in bits {
            if bit >= *old_width {
                return Err(TypeError::CannotSetBitIndex(bit, *old_width));
            }

            if !used.insert(bit) {
                return Err(TypeError::DuplicateBitIndex(bit));
            }
            used.insert(bit);
        }

        return Ok(Self::Bits(used.len()));
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
            Self::Uninitialized => write!(f, "uninitialized"),
            Self::Unknown => write!(f, "unknown"),
            Self::NamedUnknown(name) => write!(f, "{name}"),
            Self::Any => write!(f, "any"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        file_system::FileRange,
        symbol_map::{SymbolMap, class::Class, def::Def, record::RecordId, typ::Type},
    };

    #[test]
    fn bits_width() {
        let ty = Type::Bits(4);
        assert!(ty.with_bits(vec![0]).is_ok());
        assert!(ty.with_bits(vec![0, 1, 2, 3]).is_ok());
        assert!(ty.with_bits(vec![4]).is_err());
        assert!(ty.with_bits(vec![0, 1, 2, 3, 4]).is_err());
        assert!(ty.with_bits(vec![0, 0]).is_err());

        let ty = Type::String;
        assert!(ty.with_bits(vec![0]).is_err());
    }

    #[test]
    fn record() {
        let mut symbol_map = SymbolMap::default();

        let base_class = Class::new("base".into(), FileRange::detached());
        let base_class_id = symbol_map
            .add_class(base_class)
            .expect("failed to add class");

        let mut derived_class = Class::new("derived".into(), FileRange::detached());
        derived_class.add_parent(base_class_id);
        let derived_class_id = symbol_map
            .add_class(derived_class)
            .expect("failed to add class");

        let mut base_def = Def::new("base_val".into(), FileRange::detached());
        base_def.add_parent(base_class_id);
        let base_def_id = symbol_map
            .add_def(base_def, false)
            .expect("failed to add def");

        let mut derived_def = Def::new("derived_val".into(), FileRange::detached());
        derived_def.add_parent(derived_class_id);
        let derived_def_id = symbol_map
            .add_def(derived_def, false)
            .expect("failed to add def");

        let base_class_ty = Type::Record(RecordId::Class(base_class_id), "base".into());
        let derived_class_ty = Type::Record(RecordId::Class(derived_class_id), "derived".into());
        let base_def_ty = Type::Record(RecordId::Def(base_def_id), "base_val".into());
        let derived_def_ty = Type::Record(RecordId::Def(derived_def_id), "derived_val".into());

        assert!(base_class_ty.can_be_casted_to(&symbol_map, &base_class_ty));
        assert!(!base_class_ty.can_be_casted_to(&symbol_map, &base_def_ty));
        assert!(base_def_ty.can_be_casted_to(&symbol_map, &base_def_ty));
        assert!(base_def_ty.can_be_casted_to(&symbol_map, &base_class_ty));

        assert!(derived_class_ty.can_be_casted_to(&symbol_map, &derived_class_ty));
        assert!(!derived_class_ty.can_be_casted_to(&symbol_map, &base_def_ty));
        assert!(derived_def_ty.can_be_casted_to(&symbol_map, &derived_def_ty));
        assert!(derived_def_ty.can_be_casted_to(&symbol_map, &derived_class_ty));

        assert!(!base_class_ty.can_be_casted_to(&symbol_map, &derived_class_ty));
        assert!(!base_class_ty.can_be_casted_to(&symbol_map, &derived_def_ty));
        assert!(!base_def_ty.can_be_casted_to(&symbol_map, &derived_class_ty));
        assert!(!base_def_ty.can_be_casted_to(&symbol_map, &derived_def_ty));

        assert!(derived_class_ty.can_be_casted_to(&symbol_map, &base_class_ty));
        assert!(!derived_class_ty.can_be_casted_to(&symbol_map, &base_def_ty));
        assert!(derived_def_ty.can_be_casted_to(&symbol_map, &base_class_ty));
        assert!(derived_def_ty.can_be_casted_to(&symbol_map, &base_def_ty));
    }
}
