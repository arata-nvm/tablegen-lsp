use std::fs;

use tablegen_parser::{lexer::Lexer, token::TokenKind};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("usage: tablegen-parse <file>");
        return;
    }

    let text = fs::read_to_string(&args[1]).unwrap();
    let mut lexer = Lexer::new(&text);
    loop {
        let token = lexer.next();
        let cursor = lexer.cursor();
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
