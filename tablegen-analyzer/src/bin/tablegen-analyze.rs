use std::fs;

use tablegen_analyzer::document::{Document, DocumentId};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 5 {
        println!("usage: tablegen-analyze [def_loc|ref_loc|hover] <file> <line> <char>");
        return;
    }

    let text = fs::read_to_string(&args[2]).unwrap();
    let doc = Document::parse(DocumentId::new(0), text);

    let line: usize = args[3].parse().unwrap();
    let char_: usize = args[4].parse().unwrap();
    let pos = doc.line_to_pos(line).unwrap() + char_;

    match args[1].as_str() {
        "def_loc" => println!("{:?}", doc.get_definition(pos)),
        "ref_loc" => println!("{:?}", doc.get_references(pos)),
        "hover" => println!("{:?}", doc.get_hover(pos)),
        _ => unimplemented!(),
    }
}
