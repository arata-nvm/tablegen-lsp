use ecow::EcoString;
use syntax::parser::TextRange;

use crate::eval::EvalDatabase;
use crate::file_system::FileId;
use crate::symbol_map::SymbolId;

pub fn exec(db: &dyn EvalDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();

    let Some(iter) = symbol_map.iter_symbol_in(file_id) else {
        tracing::info!("no classes found in file: {file_id:?}");
        return None;
    };

    let mut symbols = Vec::new();
    for symbol_id in iter {
        match symbol_id {
            SymbolId::ClassId(class_id) => {
                let class = symbol_map.class(class_id);
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
                    .map(|id| symbol_map.field(id))
                    .map(|field| DocumentSymbol {
                        name: field.name.clone(),
                        typ: field.typ.to_string().into(),
                        range: field.define_loc.range,
                        kind: DocumentSymbolKind::Field,
                        children: Vec::new(),
                    });

                symbols.push(DocumentSymbol {
                    name: class.name.clone(),
                    typ: "class".into(),
                    range: class.define_loc.range,
                    kind: DocumentSymbolKind::Class,
                    children: template_argument_list.chain(field_list).collect(),
                });
            }
            SymbolId::DefId(def_id) => {
                let def = symbol_map.def(def_id);
                let field_list = def
                    .iter_field()
                    .map(|id| symbol_map.field(id))
                    .map(|field| DocumentSymbol {
                        name: field.name.clone(),
                        typ: field.typ.to_string().into(),
                        range: field.define_loc.range,
                        kind: DocumentSymbolKind::Field,
                        children: Vec::new(),
                    });

                symbols.push(DocumentSymbol {
                    name: def.name.clone(),
                    typ: "def".into(),
                    range: def.define_loc.range,
                    kind: DocumentSymbolKind::Def,
                    children: field_list.collect(),
                });
            }
            _ => {}
        }
    }
    Some(symbols)
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
