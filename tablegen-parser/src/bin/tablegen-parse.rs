use std::fs;

use tablegen_parser::{kind::TokenKind, lexer::Lexer};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("usage: tablegen-parse <file>");
        return;
    }

    let text = fs::read_to_string(&args[1]).unwrap();
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
