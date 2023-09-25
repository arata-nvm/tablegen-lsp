use ecow::EcoString;
use tablegen_parser::ast;

use crate::document::{symbol::Location, TableGenDocument};

#[derive(Debug)]
pub struct Symbol {
    pub name: EcoString,
    pub define_loc: Location,
    pub kind: SymbolKind,
    pub children: Vec<Symbol>,
}

#[derive(Debug)]
pub enum SymbolKind {
    Class,
    TemplateArg,
    Field,
}

impl Symbol {
    fn new(name: EcoString, define_loc: Location, kind: SymbolKind) -> Self {
        Self {
            name,
            define_loc,
            kind,
            children: Vec::new(),
        }
    }
}

pub fn document_symbol(doc: &TableGenDocument) -> Option<Vec<Symbol>> {
    let mut symbols = Vec::new();

    let node = doc.root();
    let file = node.cast::<ast::File>()?;
    let stmt_list = file.statement_list()?;
    for stmt in stmt_list.statements() {
        match stmt {
            ast::Statement::Class(class) => {
                let Some(id) = class.name() else { continue; };
                let Some(name) = id.value() else { continue; };
                let mut class_symbol =
                    Symbol::new(name.clone(), (doc.id(), id.range()), SymbolKind::Class);

                if let Some(template_arg_list) = class.template_arg_list() {
                    for arg in template_arg_list.args() {
                        let Some(id) = arg.name() else { continue; };
                        let Some(name) = id.value() else { continue; };

                        class_symbol.children.push(Symbol::new(
                            name.clone(),
                            (doc.id(), id.range()),
                            SymbolKind::TemplateArg,
                        ));
                    }
                }

                if let Some(record_body) = class.record_body() {
                    let Some(body) = record_body.body() else { continue; };
                    for item in body.items() {
                        match item {
                            ast::BodyItem::FieldDef(field_def) => {
                                let Some(id) = field_def.name() else { continue; };
                                let Some(name) = id.value() else { continue; };

                                class_symbol.children.push(Symbol::new(
                                    name.clone(),
                                    (doc.id(), id.range()),
                                    SymbolKind::Field,
                                ));
                            }
                            _ => {}
                        }
                    }
                }

                symbols.push(class_symbol);
            }
            _ => {}
        }
    }

    Some(symbols)
}
