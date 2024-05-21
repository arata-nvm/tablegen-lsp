use ecow::EcoString;
use syntax::parser::TextRange;

use crate::eval::EvalDatabase;
use crate::file_system::FileId;

pub fn exec(db: &dyn EvalDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();

    let Some(iter) = symbol_map.iter_class_in(file_id) else {
        tracing::info!("no classes found in file: {file_id:?}");
        return None;
    };

    let mut symbols = Vec::new();
    for class_id in iter {
        let class = symbol_map.class(class_id);
        let template_argument_list = class
            .template_arg_list
            .iter()
            .map(|id| symbol_map.template_arg(*id))
            .map(|arg| DocumentSymbol {
                name: arg.name.clone(),
                typ: arg.typ.to_string().into(),
                range: arg.define_loc.range,
                kind: DocumentSymbolKind::TemplateArgument,
                children: Vec::new(),
            });

        let field_list = class
            .field_list
            .iter()
            .map(|id| symbol_map.field(*id))
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
}

#[cfg(test)]
mod tests {
    use crate::tests;

    #[test]
    fn single_file() {
        let (db, f) = tests::single_file(
            r#"
class Foo<int size> {
    int field;
}

class Bar;
"#,
        );
        let symbols = super::exec(&db, f.root_file());
        insta::assert_debug_snapshot!(symbols);
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
        let (db, f) = tests::single_file(
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
        );
        let symbols = super::exec(&db, f.root_file());
        insta::assert_debug_snapshot!(symbols);
    }
}
