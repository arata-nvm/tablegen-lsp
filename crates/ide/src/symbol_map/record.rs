use std::collections::HashMap;
use std::fmt::Debug;

use ecow::EcoString;

use indexmap::IndexMap;
use syntax::parser::TextRange;
use thiserror::Error;

use crate::eval::context::EvalCtx;
use crate::file_system::FileRange;
use crate::symbol_map::def::DefField;

use super::class::Class;
use super::class::ClassId;
use super::def::Def;
use super::expr::Expr;
use super::field::Field;
use super::field::FieldId;
use super::template_arg::TemplateArgument;
use super::template_arg::TemplateArgumentId;
use super::typ::Type;
use super::SymbolMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_template_arg: IndexMap<EcoString, TemplateArgumentId>,
    pub name_to_field: IndexMap<EcoString, FieldId>,
    pub parent_class_list: Vec<ClassId>,
}

#[derive(Debug, Error)]
pub enum RecordError {
    #[error("type of field '{0}' is incompatible: '{1}' and '{2}'")]
    IncompatibleType(EcoString, Type, Type),
}

impl Record {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            name_to_template_arg: IndexMap::new(),
            name_to_field: IndexMap::new(),
            parent_class_list: Vec::new(),
        }
    }

    pub fn add_template_arg(
        &mut self,
        symbol_map: &mut SymbolMap,
        template_arg: TemplateArgument,
    ) -> TemplateArgumentId {
        let name = template_arg.name.clone();
        let id = symbol_map.add_template_argument(template_arg);
        self.name_to_template_arg.insert(name, id);
        id
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn find_template_arg(&self, name: &EcoString) -> Option<TemplateArgumentId> {
        self.name_to_template_arg.get(name).copied()
    }

    pub fn add_field(
        &mut self,
        symbol_map: &mut SymbolMap,
        new_field: Field,
    ) -> Result<FieldId, (TextRange, RecordError)> {
        if let Some(old_field_id) = self.name_to_field.get(&new_field.name) {
            let old_field = symbol_map.field(*old_field_id);
            if old_field.typ != new_field.typ {
                return Err((
                    new_field.define_loc.range,
                    RecordError::IncompatibleType(
                        new_field.name.clone(),
                        old_field.typ.clone(),
                        new_field.typ.clone(),
                    ),
                ));
            }
        }

        let name = new_field.name.clone();
        let id = symbol_map.add_field(new_field);
        self.name_to_field.insert(name, id);
        Ok(id)
    }

    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_field.get(name).copied()
    }

    pub fn inherit(
        &mut self,
        symbol_map: &mut SymbolMap,
        parent_class_id: ClassId,
        arg_value_list: Vec<Expr>,
        reference_loc: FileRange,
    ) -> Result<(), (TextRange, RecordError)> {
        symbol_map.add_reference(parent_class_id, reference_loc);
        self.parent_class_list.push(parent_class_id);

        let mut new_field_list = Vec::new();
        let parent_class = symbol_map.class(parent_class_id);

        let mut replacement = HashMap::new();
        let mut iter_arg_value_list = arg_value_list.into_iter();
        for template_arg_id in parent_class.iter_template_arg() {
            let template_arg_value = match iter_arg_value_list.next() {
                Some(arg_value) => arg_value,
                None => {
                    let template_arg = symbol_map.template_arg(template_arg_id);
                    template_arg
                        .default_value
                        .clone()
                        .unwrap_or(Expr::uninitialized(reference_loc))
                }
            };
            replacement.insert(template_arg_id, template_arg_value);
        }

        for field_id in parent_class.name_to_field.values() {
            let mut new_field = symbol_map.field(*field_id).clone();
            new_field.expr = new_field.expr.replaced(&replacement);
            new_field_list.push(new_field)
        }

        for new_field in new_field_list {
            self.add_field(symbol_map, new_field)?;
        }

        Ok(())
    }

    pub fn into_class(self) -> Class {
        Class {
            name: self.name,
            define_loc: self.define_loc,
            reference_locs: self.reference_locs,
            name_to_template_arg: self.name_to_template_arg,
            name_to_field: self.name_to_field,
            parent_class_list: self.parent_class_list,
        }
    }

    pub fn into_def(self, ctx: &mut EvalCtx) -> Def {
        assert!(self.name_to_template_arg.is_empty());

        let mut name_to_def_field = IndexMap::new();
        for (name, field_id) in self.name_to_field.into_iter() {
            let field = ctx.symbol_map.field(field_id).clone();
            let def_field = DefField::from_field(field, ctx);
            let def_field_id = ctx.symbol_map.add_def_field(def_field);
            name_to_def_field.insert(name, def_field_id);
        }

        Def {
            name: self.name,
            define_loc: self.define_loc,
            reference_locs: self.reference_locs,
            name_to_def_field,
            parent_class_list: self.parent_class_list,
        }
    }
}
