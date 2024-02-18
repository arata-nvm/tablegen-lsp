use std::fs;

use syntax::{lexer::Lexer, parse, token_kind::TokenKind, token_stream::TokenStream};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        println!("usage: tablegen-parse [token|node|error] <file>");
        return;
    }

    let text = fs::read_to_string(&args[2]).unwrap();
    match args[1].as_str() {
        "token" => token(&text),
        "node" => node(&text),
        "error" => error(&text),
        _ => unimplemented!(),
    }
}

fn token(text: &str) {
    let mut lexer = Lexer::new(text);
    loop {
        let range = lexer.peek_range();
        let token = lexer.eat();
        if let Some(err) = lexer.take_error() {
            println!("{range:?}: Error({err})");
        } else {
            println!("{range:?}: {:?}", token);
        }

        if token == TokenKind::Eof {
            break;
        }
    }
}

fn node(text: &str) {
    let root_node = parse(text).syntax_node();
    println!("{root_node:#?}");
}

fn error(text: &str) {
    for error in parse(text).errors() {
        println!("{error}");
    }
}
