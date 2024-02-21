use ecow::EcoString;

use crate::eval::EvalDatabase;
use crate::file::FileId;

pub fn document_symbol(db: &dyn EvalDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let mut symbols = Vec::new();

    let symbol_map = db.symbol_map(file_id);
    for (_, class) in symbol_map.iter_class() {
        symbols.push(DocumentSymbol {
            name: class.name.clone(),
        });
    }

    Some(symbols)
}

pub struct DocumentSymbol {
    pub name: EcoString,
}
