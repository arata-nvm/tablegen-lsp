use std::collections::HashSet;

use ecow::EcoString;

use super::{
    SymbolMap,
    record::{RecordFieldId, RecordId},
};

#[macro_export]
macro_rules! TY {
    [bit] => {$crate::symbol_map::typ::Type::bit()};
    [int] => {$crate::symbol_map::typ::Type::int()};
    [string] => {$crate::symbol_map::typ::Type::string()};
    [code] => {$crate::symbol_map::typ::Type::code()};
    [dag] => {$crate::symbol_map::typ::Type::dag()};
    [bits<$len:tt>] => {$crate::symbol_map::typ::Type::bits($len)};
    [list<any>] => {$crate::symbol_map::typ::Type::list_any()};
    [list<$elm_typ:tt>] => {$crate::symbol_map::typ::Type::list(TY![$elm_typ])};
    [?] => {$crate::symbol_map::typ::Type::uninitialized()};
}

// TODO: Eq and isa may cause confusion
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit {
        _priv: (),
    },
    Int {
        _priv: (),
    },
    String {
        _priv: (),
    },
    Code {
        _priv: (),
    },
    Dag {
        _priv: (),
    },
    Bits {
        len: usize,
        _priv: (),
    },
    List {
        elm: Box<Type>,
        _priv: (),
    },
    Record {
        id: RecordId,
        name: EcoString,
        _priv: (),
    },
    Uninitialized {
        _priv: (),
    },
    // for error recovery
    Unknown {
        _priv: (),
    },
    // for error recovery
    NamedUnknown {
        _priv: (),
        name: EcoString,
    },
    // for empty list
    Any {
        _priv: (),
    },
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("expected bits type, got '{0}'")]
    ExpectedBitsType(Type),
    #[error("cannot set bit index {0} for bits<{1}>")]
    CannotSetBitIndex(usize, usize),
    #[error("duplicate bit index {0}")]
    DuplicateBitIndex(usize),
    #[error("expected list type, got '{0}'")]
    ExpectedListType(Type),
}

impl Type {
    pub fn bit() -> Self {
        Self::Bit { _priv: () }
    }

    pub fn int() -> Self {
        Self::Int { _priv: () }
    }

    pub fn string() -> Self {
        Self::String { _priv: () }
    }

    pub fn code() -> Self {
        Self::Code { _priv: () }
    }

    pub fn dag() -> Self {
        Self::Dag { _priv: () }
    }

    pub fn bits(len: usize) -> Self {
        Self::Bits { len, _priv: () }
    }

    pub fn list(elm: Type) -> Self {
        Self::List {
            elm: Box::new(elm),
            _priv: (),
        }
    }

    pub fn list_any() -> Self {
        Self::list(Self::Any { _priv: () })
    }

    pub fn record(id: RecordId, name: EcoString) -> Self {
        Self::Record {
            id,
            name,
            _priv: (),
        }
    }

    pub fn uninitialized() -> Self {
        Self::Uninitialized { _priv: () }
    }

    pub fn unknown() -> Self {
        Self::Unknown { _priv: () }
    }

    pub fn named_unknown(name: EcoString) -> Self {
        Self::NamedUnknown { name, _priv: () }
    }
}

impl Type {
    pub fn is_bits(&self) -> bool {
        matches!(
            self,
            Self::Bits { len: _, _priv: _ } | Self::Uninitialized { _priv: _ }
        )
    }

    pub fn is_list(&self) -> bool {
        matches!(
            self,
            Self::List { elm: _, _priv: _ } | Self::Uninitialized { _priv: _ }
        )
    }

    pub fn is_record(&self) -> bool {
        matches!(
            self,
            Self::Record {
                id: _,
                name: _,
                _priv: _
            } | Self::Uninitialized { _priv: _ }
        )
    }

    pub fn list_element_type(&self) -> Result<Type, TypeError> {
        match self {
            Self::List { elm, _priv: _ } => Ok(*elm.clone()),
            _ => Err(TypeError::ExpectedListType(self.clone())),
        }
    }

    pub fn record_find_field(
        &self,
        symbol_map: &SymbolMap,
        name: &EcoString,
    ) -> Option<RecordFieldId> {
        let Self::Record {
            id: record_id,
            name: _,
            _priv: _,
        } = self
        else {
            return None;
        };

        let record = symbol_map.record(*record_id);
        record.find_field(symbol_map, name)
    }

    pub fn bits_with_selected_indices(&self, bits: Vec<usize>) -> Result<Self, TypeError> {
        let Self::Bits {
            len: old_width,
            _priv: _,
        } = self
        else {
            return Err(TypeError::ExpectedBitsType(self.clone()));
        };

        let mut used = HashSet::new();
        for bit in bits {
            if bit >= *old_width {
                return Err(TypeError::CannotSetBitIndex(bit, *old_width));
            }

            if !used.insert(bit) {
                return Err(TypeError::DuplicateBitIndex(bit));
            }
        }

        return Ok(Self::bits(used.len()));
    }

    pub fn can_be_casted_to(&self, symbol_map: &SymbolMap, other: &Type) -> bool {
        match (self, other) {
            (
                Self::Uninitialized { _priv: _ }
                | Self::Unknown { _priv: _ }
                | Self::NamedUnknown { _priv: _, .. }
                | Self::Any { _priv: _ },
                _,
            )
            | (
                _,
                Self::Uninitialized { _priv: _ }
                | Self::Unknown { _priv: _ }
                | Self::NamedUnknown { _priv: _, .. }
                | Self::Any { _priv: _ },
            ) => true,
            // 0,1以外の値の場合はエラーを出す必要がある
            (Self::Int { _priv: _ }, Self::Bit { _priv: _ })
            | (Self::Bit { _priv: _ }, Self::Int { _priv: _ }) => true,
            // 指定されたビット幅でIntを表現できない場合はエラーを出す必要がある
            (Self::Int { _priv: _ }, Self::Bits { len: _, _priv: _ })
            | (Self::Bits { len: _, _priv: _ }, Self::Int { _priv: _ }) => true,
            (Self::Bit { _priv: _ }, Self::Bits { len: 1, _priv: _ })
            | (Self::Bits { len: 1, _priv: _ }, Self::Bit { _priv: _ }) => true,
            (Self::String { _priv: _ }, Self::Code { _priv: _ })
            | (Self::Code { _priv: _ }, Self::String { _priv: _ }) => true,
            (
                Self::List {
                    elm: self_elm_typ,
                    _priv: _,
                },
                Self::List {
                    elm: other_elm_typ,
                    _priv: _,
                },
            ) => self_elm_typ.can_be_casted_to(symbol_map, other_elm_typ),
            (
                Self::Record {
                    id: self_record_id,
                    name: _,
                    _priv: _,
                },
                Self::Record {
                    id: other_record_id,
                    name: _,
                    _priv: _,
                },
            ) => {
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
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bit { _priv: _ } => write!(f, "bit"),
            Self::Int { _priv: _ } => write!(f, "int"),
            Self::String { _priv: _ } => write!(f, "string"),
            Self::Code { _priv: _ } => write!(f, "code"),
            Self::Dag { _priv: _ } => write!(f, "dag"),
            Self::Bits {
                len: width,
                _priv: _,
            } => write!(f, "bits<{width}>"),
            Self::List { elm: typ, _priv: _ } => write!(f, "list<{typ}>"),
            Self::Record {
                id: _,
                name,
                _priv: _,
            } => write!(f, "{name}"),
            Self::Uninitialized { _priv: _ } => write!(f, "uninitialized"),
            Self::Unknown { _priv: _ } => write!(f, "unknown"),
            Self::NamedUnknown { name, _priv: _ } => write!(f, "{name}"),
            Self::Any { _priv: _ } => write!(f, "any"),
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
        let ty = Type::bits(4);
        assert!(ty.bits_with_selected_indices(vec![0]).is_ok());
        assert!(ty.bits_with_selected_indices(vec![0, 1, 2, 3]).is_ok());
        assert!(ty.bits_with_selected_indices(vec![4]).is_err());
        assert!(ty.bits_with_selected_indices(vec![0, 1, 2, 3, 4]).is_err());
        assert!(ty.bits_with_selected_indices(vec![0, 0]).is_err());

        let ty = Type::string();
        assert!(ty.bits_with_selected_indices(vec![0]).is_err());
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

        let base_class_ty = Type::record(RecordId::Class(base_class_id), "base".into());
        let derived_class_ty = Type::record(RecordId::Class(derived_class_id), "derived".into());
        let base_def_ty = Type::record(RecordId::Def(base_def_id), "base_val".into());
        let derived_def_ty = Type::record(RecordId::Def(derived_def_id), "derived_val".into());

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
