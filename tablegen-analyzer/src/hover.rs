use tablegen_parser::{error::Range, kind::TokenKind, linked_node::LinkedNode, node::SyntaxNode};

pub fn hover(range: Range, root: &SyntaxNode) -> Option<String> {
    let node = LinkedNode::new(root);
    let id_node = node.find(range)?;
    let class_node = id_node.parent()?;

    let mut sibling = class_node.prev_sibling()?;
    let mut comments = Vec::new();
    loop {
        if sibling.node().token_kind() != TokenKind::Whitespace || sibling.node().text() != "\n" {
            break;
        }

        sibling = sibling.prev_sibling()?;
        if sibling.node().token_kind() != TokenKind::LineComment {
            break;
        }

        let comment = sibling.node().text();
        let comment_content = comment.trim_start_matches("// ").to_string();
        comments.push(comment_content);
        sibling = sibling.prev_sibling()?;
    }

    comments
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n")
        .into()
}
