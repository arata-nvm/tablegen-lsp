use std::fs;

use tablegen_analyzer::document::{Document, DocumentId};
use tablegen_parser::parser::TextSize;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        println!("usage: tablegen-analyze [def|ref|hover] <file>");
        return;
    }

    let text = fs::read_to_string(&args[2]).unwrap();
    let doc = Document::parse(DocumentId::new(0), &text);

    fn convert_pos(pos: TextSize, doc: &Document) -> (usize, usize) {
        let line = doc.pos_to_line(pos).unwrap();
        let char_ = pos - doc.line_to_pos(line).unwrap();
        (line + 1, Into::<usize>::into(char_) + 1)
    }


    match args[1].as_str() {
        "def" => {
            println!("--- definitions ---");
            for cursor_pos in 0..text.len() {
                let cursor_pos: TextSize = cursor_pos.try_into().unwrap();
                let Some(symbol) = doc.symbol_map().get_symbol_at(cursor_pos) else { continue; };
                let (doc_id, range) = symbol.define_loc();
                println!(
                        "{cursor:3?} -> {symbol_start:3?}..{symbol_end:3?}: {symbol_name} @ {symbol_doc:?}",
                        cursor = convert_pos(cursor_pos, &doc),
                        symbol_start = convert_pos(range.start(), &doc),
                        symbol_end = convert_pos(range.end(), &doc), 
                        symbol_name = symbol.name(),
                        symbol_doc = doc_id,
                    );
            }
        }
        "ref" => {
            println!("--- references ---");
            for cursor_pos in 0..text.len() {
                let cursor_pos: TextSize = cursor_pos.try_into().unwrap();
                let Some(symbol) = doc.symbol_map().get_symbol_at(cursor_pos) else { continue; };
                let refs = symbol.reference_locs();
                println!("{cursor:3?} -> {symbol_name} @ {symbol_doc:?}:", 
                    cursor = convert_pos(cursor_pos, &doc),
                    symbol_name = symbol.name(),
                    symbol_doc = symbol.define_loc().0,
                );
                for (doc_id, range) in refs {
                    println!(
                        "    {ref_start:3?}..{ref_end:3?} @ {doc_id:?}",
                        ref_start = convert_pos(range.start(), &doc),
                        ref_end = convert_pos(range.end(), &doc), 
                    );
                }
            }
        }
        "hover" => {
            println!("--- hovers ---");
            for cursor_pos in 0..text.len() {
                let cursor_pos: TextSize = cursor_pos.try_into().unwrap();
                let Some(hover) = doc.get_hover(cursor_pos) else { continue; };
                println!("{cursor:3?} -> {hover:?}", 
                    cursor = convert_pos(cursor_pos, &doc),
                    hover = hover,
                );
            }
        }
        _ => unimplemented!(),
    }
}
