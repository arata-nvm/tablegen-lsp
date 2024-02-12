use std::fs;
use std::path::PathBuf;

use tablegen_analyzer::server_impl::TableGenLanguageServerImpl;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("usage: tablegen-analyze <file>");
        return;
    }

    let mut impl_ = TableGenLanguageServerImpl::new();

    let path = PathBuf::from(&args[1]);
    let text = fs::read_to_string(&path).unwrap();
    let errors = impl_.check_file(path.clone(), text);

    println!("--- errors ---");
    for error in errors {
        println!("{error:?}")
    }

    println!("--- symbols ---");
    impl_.with_document(path, |_, document| {
        for symbol in document.symbol_map().document_symbols() {
            print!("{}", symbol.name());
            let Some(record) = symbol.as_record() else {
                println!();
                continue;
            };

            let parents = record
                .parents()
                .iter()
                .filter_map(|symbol_id| document.symbol_map().symbol(*symbol_id))
                .map(|symbol| symbol.name().to_string())
                .collect::<Vec<String>>()
                .join(", ");

            let template_args = record
                .template_args()
                .iter()
                .filter_map(|symbol_id| document.symbol_map().symbol(*symbol_id))
                .map(|symbol| symbol.name().to_string())
                .collect::<Vec<String>>()
                .join(", ");

            let fields = record
                .fields()
                .iter()
                .filter_map(|symbol_id| document.symbol_map().symbol(*symbol_id))
                .filter_map(|symbol| symbol.as_field())
                .map(|field| field.name().to_string())
                .collect::<Vec<String>>()
                .join(";\n");

            println!("<{template_args}>: {parents} {{\n{fields}}}");
        }
        Some(())
    });
}
