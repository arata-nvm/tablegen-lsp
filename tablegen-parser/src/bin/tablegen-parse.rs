use std::fs;

use tablegen_parser::{grammar::parse, kind::TokenKind, lexer::Lexer};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        println!("usage: tablegen-parse [token|node] <file>");
        return;
    }

    let text = fs::read_to_string(&args[2]).unwrap();
    match args[1].as_str() {
        "token" => token(&text),
        "node" => node(&text),
        _ => unimplemented!(),
    }
}

fn token(text: &str) {
    let mut lexer = Lexer::new(&text);
    loop {
        let cursor = lexer.cursor();
        let token = lexer.next();
        if let Some(err) = lexer.take_error() {
            println!("{cursor}: Error({err})");
        } else {
            println!("{cursor}: {:?}", token);
        }

        if token == TokenKind::Eof {
            break;
        }
    }
}

fn node(text: &str) {
    let node = parse(&text);
    println!("{node}");
}
