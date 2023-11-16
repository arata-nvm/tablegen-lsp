use std::fs;

use tablegen_evaluator::eval::evaluate;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("usage: tablegen-evaluate <file>");
        return;
    }

    let text = fs::read_to_string(&args[1]).unwrap();
    let root = tablegen_parser::grammar::parse(&text);
    let record_keeper = evaluate(root);
    println!("{record_keeper:#?}");
}
