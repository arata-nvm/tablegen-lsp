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
                range: arg.define_loc.range,
                kind: DocumentSymbolKind::TemplateArgument,
                children: Vec::new(),
            })
            .collect();

        symbols.push(DocumentSymbol {
            name: class.name.clone(),
            range: class.define_loc.range,
            kind: DocumentSymbolKind::Class,
            children: template_argument_list,
        });
    }
    Some(symbols)
}

#[derive(Debug)]
pub struct DocumentSymbol {
    pub name: EcoString,
    pub range: TextRange,
    pub kind: DocumentSymbolKind,
    pub children: Vec<DocumentSymbol>,
}

#[derive(Debug)]
pub enum DocumentSymbolKind {
    Class,
    TemplateArgument,
}

#[cfg(test)]
mod tests {
    use crate::tests;

    #[test]
    fn single_file() {
        let (db, f) = tests::single_file("class Foo<int size>; class Bar;");
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
}
