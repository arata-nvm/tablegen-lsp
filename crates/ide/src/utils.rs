use syntax::{SyntaxNode, parser::TextRange};

pub fn range_excluding_trivia(node: &SyntaxNode) -> TextRange {
    let start = node.text_range().start();
    let mut end_token = node.last_token();
    while let Some(token) = end_token {
        if !token.kind().is_trivia() {
            return TextRange::new(start, token.text_range().end());
        }
        end_token = token.prev_token();
    }
    TextRange::empty(start)
}
