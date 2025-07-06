use ecow::EcoString;
use syntax::parser::TextRange;

use crate::db::Db;
use crate::file_system::{FileId, SourceUnitId};
use crate::index::index;
use crate::symbol_map::SymbolMap;
use crate::symbol_map::symbol::Symbol;
use crate::symbol_map::variable::VariableKind;

pub fn exec(
    db: &dyn Db,
    source_unit_id: SourceUnitId,
    file_id: FileId,
) -> Option<Vec<DocumentSymbol>> {
    let index = index(db, source_unit_id);
    let symbol_map = index.symbol_map();

    let Some(iter) = symbol_map.iter_symbols_in_file(file_id) else {
        tracing::info!("no symbols found in file: {file_id:?}");
        return Some(vec![]);
    };

    let mut symbols = Vec::new();
    for symbol_id in iter {
        let symbol = symbol_map.symbol(symbol_id);
        if let Some(document_symbol) = symbol_to_document_symbol(symbol_map, symbol) {
            symbols.push(document_symbol);
        }
    }
    Some(symbols)
}

fn symbol_to_document_symbol(symbol_map: &SymbolMap, symbol: Symbol) -> Option<DocumentSymbol> {
    match symbol {
        Symbol::Class(class) => {
            let template_argument_list = class
                .iter_template_arg()
                .map(|id| symbol_map.template_arg(id))
                .map(|arg| DocumentSymbol {
                    name: arg.name.clone(),
                    typ: arg.typ.to_string().into(),
                    range: arg.define_loc.range,
                    kind: DocumentSymbolKind::TemplateArgument,
                    children: Vec::new(),
                });

            let field_list = class
                .iter_field()
                .map(|id| symbol_map.record_field(id))
                .map(|field| DocumentSymbol {
                    name: field.name.clone(),
                    typ: field.typ.to_string().into(),
                    range: field.define_loc.range,
                    kind: DocumentSymbolKind::Field,
                    children: Vec::new(),
                });

            Some(DocumentSymbol {
                name: class.name.clone(),
                typ: "class".into(),
                range: class.define_loc.range,
                kind: DocumentSymbolKind::Class,
                children: template_argument_list.chain(field_list).collect(),
            })
        }
        Symbol::Def(def) => {
            let field_list = def
                .iter_field()
                .map(|id| symbol_map.record_field(id))
                .map(|field| DocumentSymbol {
                    name: field.name.clone(),
                    typ: field.typ.to_string().into(),
                    range: field.define_loc.range,
                    kind: DocumentSymbolKind::Field,
                    children: Vec::new(),
                });

            Some(DocumentSymbol {
                name: def.name.clone(),
                typ: "def".into(),
                range: def.define_loc.range,
                kind: DocumentSymbolKind::Def,
                children: field_list.collect(),
            })
        }
        Symbol::Defset(defset) => {
            let def_list = defset
                .def_list
                .iter()
                .map(|id| symbol_map.symbol((*id).into()))
                .filter_map(|symbol| symbol_to_document_symbol(symbol_map, symbol));

            Some(DocumentSymbol {
                name: defset.name.clone(),
                typ: "defset".into(),
                range: defset.define_loc.range,
                kind: DocumentSymbolKind::Defset,
                children: def_list.collect(),
            })
        }
        Symbol::Multiclass(multiclass) => {
            let def_list = multiclass
                .def_list
                .iter()
                .map(|id| symbol_map.symbol((*id).into()))
                .filter_map(|symbol| symbol_to_document_symbol(symbol_map, symbol));

            Some(DocumentSymbol {
                name: multiclass.name.clone(),
                typ: "multiclass".into(),
                range: multiclass.define_loc.range,
                kind: DocumentSymbolKind::Multiclass,
                children: def_list.collect(),
            })
        }
        Symbol::Defm(defm) => Some(DocumentSymbol {
            name: defm.name.clone(),
            typ: "defm".into(),
            range: defm.define_loc.range,
            kind: DocumentSymbolKind::Defm,
            children: vec![],
        }),
        Symbol::Variable(variable) if variable.kind == VariableKind::Defvar => {
            Some(DocumentSymbol {
                name: variable.name.clone(),
                typ: "defvar".into(),
                range: variable.define_loc.range,
                kind: DocumentSymbolKind::Variable,
                children: vec![],
            })
        }
        _ => None,
    }
}

#[derive(Debug)]
pub struct DocumentSymbol {
    pub name: EcoString,
    pub typ: EcoString,
    pub range: TextRange,
    pub kind: DocumentSymbolKind,
    pub children: Vec<DocumentSymbol>,
}

#[derive(Debug)]
pub enum DocumentSymbolKind {
    Class,
    TemplateArgument,
    Field,
    Def,
    Variable,
    Defset,
    Multiclass,
    Defm,
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::DocumentSymbol;

    fn check(s: &str) -> Option<Vec<DocumentSymbol>> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.source_unit_id(), f.root_file())
    }

    #[test]
    fn single_file() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo<int size> {
    int field;
}

class Bar;
"#,
        ));
    }

    #[test]
    fn multiple_files() {
        let (db, f) = tests::multiple_files(
            r#"
; main.td
include "sub.td"
class Foo;

; sub.td
class Bar;
            "#,
        );
        let symbols = super::exec(&db, f.source_unit_id(), f.root_file());
        insta::assert_debug_snapshot!(symbols);
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(check(
            r#"
class Bar;
class Foo {
    bit a;
    int b;
    string c;
    dag d;
    bits<4> e;
    list<int> f;
    Bar g;
    code h;
}
            "#,
        ));
    }

    #[test]
    fn recursive_class() {
        insta::assert_debug_snapshot!(check(r#" class Foo { Foo g; } "#));
    }

    #[test]
    fn def() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo;
def foo : Foo {
  int a
};
"#
        ));
    }

    #[test]
    fn defset() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo;
defset list<Foo> foos = {
    def foo: Foo;
}
"#
        ));
    }

    #[test]
    fn defvar() {
        insta::assert_debug_snapshot!(check(
            r#"
defvar foo = 1;
"#
        ));
    }

    #[test]
    fn multiclass() {
        insta::assert_debug_snapshot!(check(
            r#"
multiclass Foo {
    def a;
}
defm foo: Foo;
"#
        ));
    }
}
