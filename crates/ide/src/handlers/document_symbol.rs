use ecow::EcoString;
use syntax::parser::TextRange;

use crate::eval::EvalDatabase;
use crate::file_system::FileId;
use crate::symbol_map::symbol::Symbol;
use crate::symbol_map::SymbolMap;

pub fn exec(db: &dyn EvalDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let symbol_map = db.symbol_map(file_id);

    let Some(iter) = symbol_map.iter_symbols_in_file(file_id) else {
        tracing::info!("no symbols found in file: {file_id:?}");
        return None;
    };

    let mut symbols = Vec::new();
    for symbol_id in iter {
        let symbol = symbol_map.symbol(symbol_id);
        if let Some(document_symbol) = symbol_to_document_symbol(&symbol_map, symbol) {
            symbols.push(document_symbol);
        }
    }
    Some(symbols)
}

fn symbol_to_document_symbol(symbol_map: &SymbolMap, symbol: Symbol) -> Option<DocumentSymbol> {
    match symbol {
        Symbol::Class(class) => {
            let template_args = class
                .iter_template_arg()
                .map(|arg_id| symbol_map.template_arg(arg_id))
                .map(|arg| DocumentSymbol {
                    name: arg.name.clone(),
                    typ: "".into(), // TODO
                    range: arg.define_loc.range,
                    kind: DocumentSymbolKind::TemplateArgument,
                    children: vec![],
                });

            let fields = class
                .iter_field()
                .map(|field_id| symbol_map.field(field_id))
                .map(|field| DocumentSymbol {
                    name: field.name.clone(),
                    typ: "".into(), // TODO
                    range: field.define_loc.range,
                    kind: DocumentSymbolKind::Field,
                    children: vec![],
                });

            let children = template_args.chain(fields).collect();

            Some(DocumentSymbol {
                name: class.name.clone(),
                typ: "class".into(),
                range: class.define_loc.range,
                kind: DocumentSymbolKind::Class,
                children,
            })
        }
        Symbol::Def(def) => {
            let fields = def
                .iter_field()
                .map(|field_id| symbol_map.field(field_id))
                .map(|field| DocumentSymbol {
                    name: field.name.clone(),
                    typ: "".into(), // TODO
                    range: field.define_loc.range,
                    kind: DocumentSymbolKind::Field,
                    children: vec![],
                })
                .collect();

            Some(DocumentSymbol {
                name: def.name.clone(),
                typ: "def".into(),
                range: def.define_loc.range,
                kind: DocumentSymbolKind::Def,
                children: fields,
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
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::DocumentSymbol;

    fn check(s: &str) -> Option<Vec<DocumentSymbol>> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.root_file())
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
        let symbols = super::exec(&db, f.root_file());
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
}
