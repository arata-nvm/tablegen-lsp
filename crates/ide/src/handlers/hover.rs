use syntax::parser::TextRange;
use syntax::syntax_kind::SyntaxKind;
use syntax::SyntaxNode;

use crate::file_system::{FilePosition, FileRange};
use crate::index::IndexDatabase;
use crate::symbol_map::record::RecordKind;
use crate::symbol_map::symbol::Symbol;
use crate::symbol_map::variable::VariableKind;
use crate::symbol_map::SymbolMap;

#[derive(Debug)]
pub struct Hover {
    pub signature: String,
    pub document: Option<String>,
}

pub fn exec(db: &dyn IndexDatabase, pos: FilePosition) -> Option<Hover> {
    let index = db.index();
    let symbol_map = index.symbol_map();

    let (signature, define_loc) = extract_symbol_signature(symbol_map, pos)?;

    let parse = db.parse(define_loc.file);
    let symbol_doc = extract_doc_comments(parse.syntax_node(), define_loc.range);

    Some(Hover {
        signature,
        document: symbol_doc,
    })
}

fn extract_symbol_signature(
    symbol_map: &SymbolMap,
    pos: FilePosition,
) -> Option<(String, FileRange)> {
    let symbol = symbol_map.find_symbol_at(pos)?;

    let symbol_info = match symbol {
        Symbol::Record(record) if record.kind == RecordKind::Class => {
            let name = &record.name;
            let template_arg = record
                .iter_template_arg()
                .map(|id| symbol_map.template_arg(id))
                .map(|arg| format!("{} {}", arg.typ, arg.name))
                .collect::<Vec<String>>()
                .join(", ");
            match template_arg.is_empty() {
                false => format!("class {name}<{template_arg}>"),
                true => format!("class {name}"),
            }
        }
        Symbol::Record(record) if record.kind == RecordKind::Def => {
            format!("def {}", record.name)
        }
        Symbol::Record(_) => unreachable!("unexpected record kind"),
        Symbol::TemplateArgument(template_arg) => {
            format!("{} {}", template_arg.typ, template_arg.name)
        }
        Symbol::RecordField(record_field) => {
            let parent = symbol_map.record(record_field.parent);
            format!(
                "{} {}::{}",
                record_field.typ, parent.name, record_field.name
            )
        }
        Symbol::Variable(variable) => match variable.kind {
            VariableKind::Defvar => format!("{} {}", variable.typ, variable.name),
            VariableKind::Foreach => format!("{} {}", variable.typ, variable.name),
            VariableKind::XFilter => format!("{} {}", variable.typ, variable.name),
            VariableKind::XFoldl => format!("{} {}", variable.typ, variable.name),
            VariableKind::XForeach => format!("{} {}", variable.typ, variable.name),
        },
        Symbol::Defset(defset) => {
            format!("{} {}", defset.typ, defset.name)
        }
        Symbol::Multiclass(multiclass) => {
            format!("multiclass {}", multiclass.name)
        }
        Symbol::Defm(defm) => {
            format!("defm {}", defm.name)
        }
    };

    let define_loc = symbol.define_loc();

    Some((symbol_info, *define_loc))
}

fn extract_doc_comments(root: SyntaxNode, range: TextRange) -> Option<String> {
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
    if doc.is_empty() {
        None
    } else {
        Some(doc)
    }
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::Hover;

    fn check(s: &str) -> Hover {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.marker(0)).expect("definition not found")
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(check("class $Foo;"));
    }

    #[test]
    fn template_arg() {
        insta::assert_debug_snapshot!(check("class Foo<int $arg1>;"));
    }

    #[test]
    fn field() {
        insta::assert_debug_snapshot!(check("class Foo { int $field1 = 1; }"));
    }

    #[test]
    fn doc_comment() {
        insta::assert_debug_snapshot!(check(
            r#"
// doc comment
class $Foo;
            "#
        ));
        insta::assert_debug_snapshot!(check(
            r#"
class Foo {
    // doc comment
    int $foo;
}
            "#
        ));
        insta::assert_debug_snapshot!(check(
            r#"
// doc comment
def $foo;
            "#
        ));
        insta::assert_debug_snapshot!(check(
            r#"
// doc comment

class $Foo;
            "#
        ));
    }
}
