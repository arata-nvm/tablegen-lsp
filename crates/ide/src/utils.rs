use syntax::{SyntaxNode, parser::TextRange, syntax_kind::SyntaxKind};

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

pub fn extract_doc_comments(root: SyntaxNode, range: TextRange) -> Option<String> {
    let id_node = root.covering_element(range);
    let identifier_node = match id_node.kind() {
        SyntaxKind::Id => id_node.parent()?,
        SyntaxKind::Identifier => id_node.into_node()?,
        _ => return None,
    };

    // Class or FieldDef or Defset or InnerValue
    let mut parent_node = identifier_node.parent()?;

    if parent_node.kind() == SyntaxKind::InnerValue {
        let value_node = parent_node.parent()?;
        // Def
        parent_node = value_node.parent()?;
    }

    let mut cur_token = parent_node.first_token()?;
    let mut comments = Vec::new();
    loop {
        cur_token = match cur_token.prev_token() {
            Some(t) => t,
            None => break,
        };
        if cur_token.kind() != SyntaxKind::Whitespace || cur_token.text().matches('\n').count() != 1
        {
            break;
        }

        cur_token = match cur_token.prev_token() {
            Some(t) => t,
            None => break,
        };
        if cur_token.kind() != SyntaxKind::LineComment {
            break;
        }

        let comment = cur_token.text();
        if !comment.starts_with("//") {
            break;
        }
        comments.push(comment.trim_start_matches('/').trim_start().to_string());
    }

    let doc = comments.into_iter().rev().collect::<Vec<_>>().join("\n");
    if doc.is_empty() { None } else { Some(doc) }
}
