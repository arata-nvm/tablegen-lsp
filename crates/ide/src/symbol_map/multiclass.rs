use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use super::template_arg::TemplateArgumentId;
use crate::file_system::FileRange;
use crate::symbol_map::def::DefId;

pub type MulticlassId = Id<Multiclass>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Multiclass {
    pub name: EcoString,
    pub name_to_template_arg: IndexMap<EcoString, TemplateArgumentId>,
    pub parent_list: Vec<MulticlassId>,
    pub def_list: Vec<DefId>,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Multiclass {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            name_to_template_arg: IndexMap::new(),
            parent_list: Vec::new(),
            def_list: Vec::new(),
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: TemplateArgumentId) {
        self.name_to_template_arg.insert(name, template_arg_id);
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn find_template_arg(&self, name: &EcoString) -> Option<TemplateArgumentId> {
        self.name_to_template_arg.get(name).copied()
    }

    pub fn add_parent(&mut self, parent_id: MulticlassId) {
        self.parent_list.push(parent_id);
    }

    pub fn add_def(&mut self, def_id: DefId) {
        self.def_list.push(def_id);
    }

    pub fn add_defs(&mut self, def_ids: Vec<DefId>) {
        self.def_list.extend(def_ids);
    }
}
