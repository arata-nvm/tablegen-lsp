use ecow::EcoString;

use crate::eval::EvalDatabase;
use crate::file_system::FileId;

pub fn document_symbol(db: &dyn EvalDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let evaluation = db.eval(file_id);
    let symbol_map = evaluation.symbol_map();

    let mut symbols = Vec::new();
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
