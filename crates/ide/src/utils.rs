use syntax::{
    SyntaxNode,
    ast::{self, AstNode},
    parser::TextRange,
    syntax_kind::SyntaxKind,
};

pub fn range_excluding_trivia(node: &SyntaxNode) -> TextRange {
    let start = node.text_range().start();
    let end = last_non_trivia_token(node)
        .map(|token| token.text_range().end())
        .unwrap_or(start);
    TextRange::new(start, end)
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

    let mut comments = collect_leading_doc_comments(&parent_node);
    comments.extend(collect_trailing_doc_comments(&parent_node));

    let doc = comments.join("\n");
    if doc.is_empty() { None } else { Some(doc) }
}

fn collect_leading_doc_comments(node: &SyntaxNode) -> Vec<String> {
    let Some(mut cur_token) = node.first_token() else {
        return Vec::new();
    };
    let mut comments = Vec::new();

    loop {
        let Some(whitespace) = cur_token.prev_token() else {
            break;
        };
        if !is_single_line_whitespace(&whitespace) {
            break;
        }

        let Some(comment) = whitespace.prev_token() else {
            break;
        };
        if comment.kind() != SyntaxKind::LineComment || !is_line_start(&comment) {
            break;
        }

        comments.push(strip_line_comment(comment.text()));
        cur_token = comment;
    }

    comments.into_iter().rev().collect()
}

fn collect_trailing_doc_comments(node: &SyntaxNode) -> Vec<String> {
    let mut comments = Vec::new();

    if let Some(body) = node
        .descendants()
        .find(|descendant| descendant.kind() == SyntaxKind::Body)
        && let Some(token) = body.first_token()
        && token.kind() == SyntaxKind::LBrace
        && let Some(comment) = line_comment_after(token)
    {
        comments.push(comment);
    }

    if let Some(token) = last_non_trivia_token(node)
        && let Some(comment) = line_comment_after(token)
    {
        comments.push(comment);
    }

    comments
}

fn line_comment_after(token: syntax::SyntaxToken) -> Option<String> {
    let mut cur_token = token.next_token()?;

    loop {
        match cur_token.kind() {
            SyntaxKind::Whitespace if !cur_token.text().contains('\n') => {
                cur_token = cur_token.next_token()?;
            }
            SyntaxKind::LineComment => return Some(strip_line_comment(cur_token.text())),
            _ => return None,
        }
    }
}

fn last_non_trivia_token(node: &SyntaxNode) -> Option<syntax::SyntaxToken> {
    let mut token = node.last_token();
    while let Some(current) = token {
        if !current.kind().is_trivia() {
            return Some(current);
        }
        token = current.prev_token();
    }
    None
}

fn is_single_line_whitespace(token: &syntax::SyntaxToken) -> bool {
    token.kind() == SyntaxKind::Whitespace && token.text().matches('\n').count() == 1
}

fn is_line_start(comment: &syntax::SyntaxToken) -> bool {
    match comment.prev_token() {
        None => true,
        Some(token) => token.kind() == SyntaxKind::Whitespace && token.text().contains('\n'),
    }
}

fn strip_line_comment(comment: &str) -> String {
    comment.trim_start_matches('/').trim_start().to_string()
}

#[derive(Debug)]
pub enum DefNameType {
    // def foo
    Identifier(ast::Value, ast::Identifier),
    // def foo#i
    ValueStartWithIdentifier(ast::Value),
    // def !strconcat(foo, bar)
    Value,
    // def
    Anonymous,
}

pub fn determine_def_type(def: &ast::Def) -> Option<DefNameType> {
    let Some(name_value) = def.name() else {
        return Some(DefNameType::Anonymous);
    };
    let inner_value = name_value.inner_values().next()?;
    let simple_value = inner_value.simple_value()?;
    match simple_value {
        ast::SimpleValue::Identifier(ident) => {
            if name_value.inner_values().count() > 1 {
                Some(DefNameType::ValueStartWithIdentifier(name_value))
            } else {
                Some(DefNameType::Identifier(name_value, ident))
            }
        }
        _ => Some(DefNameType::Value),
    }
}

pub(crate) trait SyntaxNodeExt {
    fn ancestor<N: AstNode<Language = syntax::Language>>(&self) -> Option<N>;

    fn ancestor_within<N: AstNode<Language = syntax::Language>>(
        &self,
        max_depth: usize,
    ) -> Option<N>;
}

impl SyntaxNodeExt for SyntaxNode {
    fn ancestor<N: AstNode<Language = syntax::Language>>(&self) -> Option<N> {
        self.ancestors().find_map(N::cast)
    }

    fn ancestor_within<N: AstNode<Language = syntax::Language>>(
        &self,
        max_depth: usize,
    ) -> Option<N> {
        self.ancestors().take(max_depth).find_map(N::cast)
    }
}

#[cfg(test)]
mod tests {
    use syntax::parser::{TextRange, TextSize};

    use crate::tests;

    use super::extract_doc_comments;

    fn check(fixture: &str, marker_count: usize) -> Vec<Option<String>> {
        let (_, fixture) = tests::single_file(fixture);
        let content = fixture.file_content(&fixture.root_file());
        let parse = syntax::parse(&content);
        let root = parse.syntax_node();

        (0..marker_count)
            .map(|index| {
                let position = fixture.marker(index).position;
                let range = TextRange::new(position, position + TextSize::from(1));
                extract_doc_comments(root.clone(), range)
            })
            .collect()
    }

    #[test]
    fn trailing_comments() {
        let comments = check(
            r#"
class Foo {
    int $first; // first comment
    int $second; // second comment
    int $without_comment;
}
            "#,
            3,
        );

        assert_eq!(comments[0].as_deref(), Some("first comment"));
        assert_eq!(comments[1].as_deref(), Some("second comment"));
        assert_eq!(comments[2], None);
    }

    #[test]
    fn leading_comments() {
        let comments = check(
            r#"
// attached comment
class $Attached;

// separated comment

class $Separated;
            "#,
            2,
        );

        assert_eq!(comments[0].as_deref(), Some("attached comment"));
        assert_eq!(comments[1], None);
    }

    #[test]
    fn block_comments() {
        let comments = check(
            r#"
class Foo {
    int $block; /* not documentation */
    int $line; // documentation
}
            "#,
            2,
        );

        assert_eq!(comments[0], None);
        assert_eq!(comments[1].as_deref(), Some("documentation"));
    }

    #[test]
    fn comment_after_brace() {
        let comments = check(
            r#"
def $foo : Bar { // comment
    let name = "foo";
}
            "#,
            1,
        );

        assert_eq!(comments[0].as_deref(), Some("comment"));
    }
}
