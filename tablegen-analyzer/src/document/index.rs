use std::collections::HashMap;

use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;
use tablegen_parser::{
    ast::{self, Identifier},
    error::Position,
    node::SyntaxNode,
};

use super::{
    symbol::{TableGenSymbol, TableGenSymbolId, TableGenSymbolKind},
    TableGenDocumentId,
};

#[derive(Debug)]
pub struct TableGenDocumentIndex {
    doc_id: TableGenDocumentId,
    symbols: Arena<TableGenSymbol>,
    symbol_map: IntervalMap<Position, TableGenSymbolId>,
}

impl TableGenDocumentIndex {
    pub fn get_symbol_at(&self, pos: Position) -> Option<&TableGenSymbol> {
        self.symbol_map
            .values_overlap(pos)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }
}

impl TableGenDocumentIndex {
    fn new(doc_id: TableGenDocumentId) -> Self {
        Self {
            doc_id,
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
        }
    }

    // TODO: wrap SyntaxNode
    pub fn create_index(doc_id: TableGenDocumentId, file: &SyntaxNode) -> Self {
        let mut index = Self::new(doc_id);

        let mut ctx = IndexContext::new();
        index.analyze_file(file, &mut ctx);
        index
    }

    fn analyze_file(&mut self, file: &SyntaxNode, ctx: &mut IndexContext) -> Option<()> {
        let file = file.cast::<ast::File>()?;
        let list = file.statement_list()?;
        for stmt in list.statements() {
            match stmt {
                ast::Statement::Class(class) => self.analyze_class(class, ctx),
                ast::Statement::Def(def) => self.analyze_def(def, ctx),
                _ => {}
            }
        }
        None
    }

    fn analyze_class(&mut self, class: ast::Class, ctx: &mut IndexContext) {
        if let Some(name) = class.name() {
            self.add_symbol(name, TableGenSymbolKind::Class, ctx);
        }

        ctx.push();
        if let Some(template_arg_list) = class.template_arg_list() {
            for arg in template_arg_list.args() {
                self.analyze_template_arg(arg, ctx);
            }
        }
        if let Some(record_body) = class.record_body() {
            self.analyze_record_body(record_body, ctx);
        }
        ctx.pop();
    }

    fn analyze_template_arg(&mut self, arg: ast::TemplateArgDecl, ctx: &mut IndexContext) {
        if let Some(name) = arg.name() {
            self.add_symbol(name, TableGenSymbolKind::TemplateArg, ctx);
        }
        if let Some(typ) = arg.r#type() {
            self.analyze_type(typ, ctx);
        }
    }

    fn analyze_record_body(&mut self, record_body: ast::RecordBody, ctx: &mut IndexContext) {
        if let Some(parent_class_list) = record_body.parent_class_list() {
            for class_ref in parent_class_list.classes() {
                self.analyze_class_ref(class_ref, ctx);
            }
        }
        if let Some(body) = record_body.body() {
            self.analyze_body(body, ctx);
        }
    }

    fn analyze_body(&mut self, body: ast::Body, ctx: &mut IndexContext) {
        for item in body.items() {
            match item {
                ast::BodyItem::FieldDef(field_def) => {
                    self.analyze_field_def(field_def, ctx);
                }
                ast::BodyItem::FieldLet(field_let) => {
                    self.analyze_field_let(field_let, ctx);
                }
            }
        }
    }

    fn analyze_field_def(&mut self, field_def: ast::FieldDef, ctx: &mut IndexContext) {
        if let Some(typ) = field_def.r#type() {
            self.analyze_type(typ, ctx);
        }
        if let Some(name) = field_def.name() {
            self.add_symbol(name, TableGenSymbolKind::Field, ctx);
        }
        if let Some(value) = field_def.value() {
            self.analyze_value(value, ctx);
        }
    }

    fn analyze_field_let(&mut self, field_let: ast::FieldLet, ctx: &mut IndexContext) {
        if let Some(value) = field_let.value() {
            self.analyze_value(value, ctx);
        }
    }

    fn analyze_def(&mut self, def: ast::Def, ctx: &mut IndexContext) {
        if let Some(name) = def.name() {
            if let Some(ast::SimpleValue::Identifier(id)) = name.simple_value() {
                self.add_symbol(id, TableGenSymbolKind::Def, ctx);
            } else {
                // TODO
            }
        }

        ctx.push();
        if let Some(record_body) = def.record_body() {
            self.analyze_record_body(record_body, ctx);
        }
        ctx.pop();
    }

    fn analyze_type(&mut self, typ: ast::Type, ctx: &mut IndexContext) -> Option<()> {
        match typ {
            ast::Type::ListType(list_typ) => {
                self.analyze_type(list_typ.inner_type()?, ctx);
            }
            ast::Type::ClassId(class_id) => {
                self.add_symbol_reference(class_id.name()?, ctx);
            }
            _ => {}
        }
        None
    }

    fn analyze_class_ref(&mut self, class_ref: ast::ClassRef, ctx: &mut IndexContext) {
        if let Some(name) = class_ref.name() {
            self.add_symbol_reference(name, ctx);
        }
        if let Some(arg_value_list) = class_ref.arg_value_list() {
            if let Some(positional) = arg_value_list.positional() {
                for arg in positional.values() {
                    self.analyze_value(arg, ctx);
                }
            }
        }
    }

    fn analyze_value(&mut self, value: ast::Value, ctx: &mut IndexContext) -> Option<()> {
        let simple_value = value.simple_value()?;
        match simple_value {
            ast::SimpleValue::Identifier(id) => {
                self.add_symbol_reference(id, ctx);
            }
            _ => {}
        }
        None
    }

    fn add_symbol(
        &mut self,
        name_id: Identifier,
        kind: TableGenSymbolKind,
        ctx: &mut IndexContext,
    ) -> Option<()> {
        let name = name_id.value()?;
        let range = name_id.range();

        let define_loc = (self.doc_id, range.clone());
        let symbol = TableGenSymbol::new(name.clone(), kind, define_loc);
        let symbol_id = self.symbols.alloc(symbol);
        self.symbol_map.insert(range, symbol_id);
        ctx.add_symbol(name.clone(), symbol_id);
        None
    }

    fn add_symbol_reference(&mut self, name_id: Identifier, ctx: &mut IndexContext) -> Option<()> {
        let name = name_id.value()?;
        let range = name_id.range();

        let reference_loc = (self.doc_id, range.clone());
        let Some(symbol_id) = ctx.find_symbol(name) else { return None; }; // TODO
        let symbol = self.symbols.get_mut(symbol_id).unwrap();
        symbol.add_reference(reference_loc);
        self.symbol_map.insert(range, symbol_id);
        None
    }
}

struct IndexContext {
    scopes: Vec<HashMap<EcoString, TableGenSymbolId>>,
}

impl IndexContext {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn add_symbol(&mut self, name: EcoString, symbol_id: TableGenSymbolId) {
        self.scopes.last_mut().unwrap().insert(name, symbol_id);
    }

    pub fn find_symbol(&self, name: &EcoString) -> Option<TableGenSymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol_id) = scope.get(name) {
                return Some(*symbol_id);
            }
        }
        None
    }
}
