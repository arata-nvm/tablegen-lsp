use ecow::EcoString;

use crate::eval::EvalDatabase;
use crate::file_system::FileId;

pub fn document_symbol(db: &dyn EvalDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();

    let Some(iter) = symbol_map.iter_class_in(file_id) else {
        tracing::info!("no classes found in file: {file_id:?}");
        return None;
    };

    let mut symbols = Vec::new();
    for class_id in iter {
        let Some(class) = symbol_map.class(class_id) else {
            tracing::info!("class not found: {class_id:?}");
            continue;
        };

        symbols.push(DocumentSymbol {
            name: class.name.clone(),
        });
    }
    Some(symbols)
}

pub struct DocumentSymbol {
    pub name: EcoString,
}
