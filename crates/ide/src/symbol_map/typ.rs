use std::collections::HashSet;

use ecow::{EcoString, eco_format};

use crate::symbol_map::class::ClassId;

use super::{
    SymbolMap,
    record::{RecordFieldId, RecordId},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RecordData {
    DefinedRecord(RecordId),
    AnonymousClass(Vec<ClassId>),
}

impl RecordData {
    fn parent_ids(&self, symbol_map: &SymbolMap) -> Vec<ClassId> {
        match self {
            Self::DefinedRecord(record_id) => {
                let record = symbol_map.record(*record_id);
                record.parents().to_vec()
            }
            Self::AnonymousClass(parents) => parents.clone(),
        }
    }

    fn is_subclass_of(&self, symbol_map: &SymbolMap, other: &RecordData) -> bool {
        fn is_subclass_of_inner(
            symbol_map: &SymbolMap,
            self_parent_ids: &[ClassId],
            other_parent_ids: &[ClassId],
        ) -> bool {
            other_parent_ids.iter().copied().all(|other_parent_id| {
                self_parent_ids.iter().any(|self_parent_id| {
                    let self_parent = symbol_map.class(*self_parent_id);
                    other_parent_id == *self_parent_id
                        || self_parent.is_subclass_of(symbol_map, other_parent_id)
                })
            })
        }

        use RecordData::*;
        match (self, other) {
            (DefinedRecord(self_id), DefinedRecord(other_id)) if self_id == other_id => true,
            (_, DefinedRecord(RecordId::Def(_))) => false,
            (DefinedRecord(self_record_id), DefinedRecord(RecordId::Class(other_class_id))) => {
                let self_record = symbol_map.record(*self_record_id);
                self_record.is_subclass_of(symbol_map, *other_class_id)
            }
            (AnonymousClass(self_parent_ids), DefinedRecord(RecordId::Class(other_class_id))) => {
                is_subclass_of_inner(symbol_map, self_parent_ids, &[*other_class_id])
            }
            (DefinedRecord(self_record_id), AnonymousClass(other_parent_ids)) => {
                let self_record = symbol_map.record(*self_record_id);
                is_subclass_of_inner(symbol_map, self_record.parents(), other_parent_ids)
            }
            (AnonymousClass(self_parent_ids), AnonymousClass(other_parent_ids)) => {
                is_subclass_of_inner(symbol_map, self_parent_ids, other_parent_ids)
            }
        }
    }
}

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
        data: RecordData,
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
            data: RecordData::DefinedRecord(id),
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
                data: _,
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
            data,
            name: _,
            _priv: _,
        } = self
        else {
            return None;
        };

        match data {
            RecordData::DefinedRecord(record_id) => {
                let record = symbol_map.record(*record_id);
                record.find_field(symbol_map, name)
            }
            RecordData::AnonymousClass(parents) => {
                for parent_id in parents {
                    let parent = symbol_map.class(*parent_id);
                    if let Some(field_id) = parent.find_field(symbol_map, name) {
                        return Some(field_id);
                    }
                }
                None
            }
        }
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

        Ok(Self::bits(used.len()))
    }

    pub fn resolve_with(&self, symbol_map: &SymbolMap, other: &Type) -> Option<Type> {
        tracing::debug!("resolving types: self={}, other={}", self, other);
        if self == other {
            tracing::debug!("  types are equal");
            return Some(self.clone());
        }

        if self.can_be_casted_to(symbol_map, other) {
            tracing::debug!("  self can be casted to other");
            return Some(other.clone());
        } else if other.can_be_casted_to(symbol_map, self) {
            tracing::debug!("  other can be casted to self");
            return Some(self.clone());
        }

        if let (
            Self::Record {
                data: self_data,
                name: _,
                _priv: _,
            },
            Self::Record {
                data: other_data,
                name: _,
                _priv: _,
            },
        ) = (self, other)
        {
            tracing::debug!("  both types are records, resolving record types");
            return Self::resolve_record_with(symbol_map, self_data, other_data);
        }

        if let (
            Self::List {
                elm: self_elm,
                _priv: _,
            },
            Self::List {
                elm: other_elm,
                _priv: _,
            },
        ) = (self, other)
        {
            tracing::debug!("  both types are lists, resolving element types");
            if let Some(resolved_elm) = self_elm.resolve_with(symbol_map, other_elm) {
                return Some(Self::list(resolved_elm));
            }
        }

        None
    }

    fn resolve_record_with(
        symbol_map: &SymbolMap,
        self_data: &RecordData,
        other_data: &RecordData,
    ) -> Option<Type> {
        tracing::debug!(
            "resolving record types: self={:?}, other={:?}",
            self_data,
            other_data
        );
        let mut common_parents = Vec::new();

        let other_parent_ids = other_data.parent_ids(symbol_map);
        let other_parents = other_parent_ids
            .into_iter()
            .map(|parent_id| (parent_id, symbol_map.class(parent_id)))
            .collect::<Vec<_>>();

        let self_parent_ids = self_data.parent_ids(symbol_map);
        let mut self_parent_stack = self_parent_ids.to_vec();
        if let RecordData::DefinedRecord(RecordId::Class(class_id)) = self_data {
            self_parent_stack.push(*class_id);
        }
        let mut visited = HashSet::new();
        while let Some(self_parent_id) = self_parent_stack.pop() {
            if !visited.insert(self_parent_id) {
                continue;
            }
            let mut found = false;
            for (other_parent_id, other_parent) in &other_parents {
                if *other_parent_id == self_parent_id
                    || other_parent.is_subclass_of(symbol_map, self_parent_id)
                {
                    common_parents.push(self_parent_id);
                    found = true;
                    break;
                }
            }

            if !found {
                let self_parent = symbol_map.class(self_parent_id);
                self_parent_stack.extend(&self_parent.parent_list);
            }
        }

        tracing::debug!("  common parents: {:?}", common_parents);

        match common_parents.len() {
            0 => None,
            1 => Some(Type::record(
                RecordId::Class(common_parents[0]),
                symbol_map.class(common_parents[0]).name.clone(),
            )),
            _ => {
                let name = common_parents
                    .iter()
                    .map(|class_id| symbol_map.class(*class_id).name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(Type::Record {
                    data: RecordData::AnonymousClass(common_parents),
                    name: eco_format!("{{{name}}}"),
                    _priv: (),
                })
            }
        }
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
                    data: self_data,
                    name: _,
                    _priv: _,
                },
                Self::Record {
                    data: other_data,
                    name: _,
                    _priv: _,
                },
            ) => self_data.is_subclass_of(symbol_map, other_data),
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
                data: _,
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
        symbol_map::{
            SymbolMap,
            class::Class,
            def::Def,
            record::RecordId,
            typ::{RecordData, Type},
        },
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
        assert!(!derived_def_ty.can_be_casted_to(&symbol_map, &base_def_ty));
    }

    #[test]
    fn transitive_subclass() {
        let mut symbol_map = SymbolMap::default();

        let grand_base_class = Class::new("GrandBase".into(), FileRange::detached());
        let grand_base_class_id = symbol_map
            .add_class(grand_base_class)
            .expect("failed to add class");

        let mut base_class = Class::new("Base".into(), FileRange::detached());
        base_class.add_parent(grand_base_class_id);
        let base_class_id = symbol_map
            .add_class(base_class)
            .expect("failed to add class");

        let mut derived_class = Class::new("Derived".into(), FileRange::detached());
        derived_class.add_parent(base_class_id);
        let derived_class_id = symbol_map
            .add_class(derived_class)
            .expect("failed to add class");

        let grand_base_ty = Type::record(RecordId::Class(grand_base_class_id), "GrandBase".into());
        let base_ty = Type::record(RecordId::Class(base_class_id), "Base".into());
        let derived_ty = Type::record(RecordId::Class(derived_class_id), "Derived".into());

        // Derived -> Base -> GrandBase
        assert!(base_ty.can_be_casted_to(&symbol_map, &grand_base_ty));
        assert!(derived_ty.can_be_casted_to(&symbol_map, &base_ty));
        assert!(derived_ty.can_be_casted_to(&symbol_map, &grand_base_ty));

        // Resolve (Derived, Base) -> Base
        let resolved = derived_ty.resolve_with(&symbol_map, &base_ty).unwrap();
        assert_eq!(resolved, base_ty);

        // Resolve (Base, Derived) -> Base
        let resolved = base_ty.resolve_with(&symbol_map, &derived_ty).unwrap();
        assert_eq!(resolved, base_ty);
    }

    #[test]
    fn anonymous_class_inheritance() {
        let mut symbol_map = SymbolMap::default();

        let base_class = Class::new("Base".into(), FileRange::detached());
        let base_class_id = symbol_map.add_class(base_class).unwrap();

        let mut derived_class = Class::new("Derived".into(), FileRange::detached());
        derived_class.add_parent(base_class_id);
        let derived_class_id = symbol_map.add_class(derived_class).unwrap();

        let other_class = Class::new("Other".into(), FileRange::detached());
        let other_class_id = symbol_map.add_class(other_class).unwrap();

        let mut a_class = Class::new("A".into(), FileRange::detached());
        a_class.add_parent(derived_class_id);
        a_class.add_parent(other_class_id);
        let a_class_id = symbol_map.add_class(a_class).unwrap();

        let mut b_class = Class::new("B".into(), FileRange::detached());
        b_class.add_parent(derived_class_id);
        b_class.add_parent(other_class_id);
        let b_class_id = symbol_map.add_class(b_class).unwrap();

        let base_ty = Type::record(RecordId::Class(base_class_id), "Base".into());
        let a_ty = Type::record(RecordId::Class(a_class_id), "A".into());
        let b_ty = Type::record(RecordId::Class(b_class_id), "B".into());

        // A & B should resolve to {Derived, Other}
        let intersection_ty = a_ty.resolve_with(&symbol_map, &b_ty).unwrap();
        let Type::Record {
            data: RecordData::AnonymousClass(ref parent_ids),
            ..
        } = intersection_ty
        else {
            panic!("expected anonymous class");
        };

        assert_eq!(parent_ids.len(), 2);
        assert!(parent_ids.contains(&derived_class_id));
        assert!(parent_ids.contains(&other_class_id));

        // {Derived, Other} is subclass of Base
        assert!(intersection_ty.can_be_casted_to(&symbol_map, &base_ty));
    }
}
