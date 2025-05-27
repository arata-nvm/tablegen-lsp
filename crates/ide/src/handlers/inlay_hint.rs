use syntax::{
    ast::{self, AstNode},
    parser::TextSize,
    syntax_kind::SyntaxKind,
};

use crate::{
    file_system::FileRange,
    index::IndexDatabase,
    symbol_map::{class::Class, record::RecordField, symbol::Symbol, SymbolMap},
};

#[derive(Debug)]
pub struct InlayHint {
    pub position: TextSize,
    pub label: String,
    pub kind: InlayHintKind,
}

#[derive(Debug)]
pub enum InlayHintKind {
    TemplateArg,
    FieldLet,
}

impl InlayHint {
    fn new(position: TextSize, label: impl Into<String>, kind: InlayHintKind) -> Self {
        Self {
            position,
            label: label.into(),
            kind,
        }
    }
}

pub fn exec(db: &dyn IndexDatabase, range: FileRange) -> Option<Vec<InlayHint>> {
    let index = db.index();
    let symbol_map = index.symbol_map();

    let Some(iter) = symbol_map.iter_symbols_in_range(range) else {
        tracing::info!("no classes found in range: {range:?}");
        return None;
    };

    let mut hints = vec![];
    for (symbol_loc, symbol_id) in iter {
        let symbol = symbol_map.symbol(symbol_id);
        match symbol {
            // TODO: 参照箇所のみをイテレートしたい
            Symbol::Class(class) => {
                if let Some(new_hints) = inlay_hint_class(db, symbol_map, class, symbol_loc) {
                    hints.extend(new_hints);
                }
            }
            Symbol::RecordField(record_field) => {
                if let Some(new_hints) = inlay_hint_record_field(db, record_field, symbol_loc) {
                    hints.extend(new_hints);
                }
            }
            _ => {}
        }
    }
    Some(hints)
}

fn inlay_hint_class(
    db: &dyn IndexDatabase,
    symbol_map: &SymbolMap,
    class: &Class,
    symbol_loc: FileRange,
) -> Option<Vec<InlayHint>> {
    let parse = db.parse(symbol_loc.file);
    let root_node = parse.syntax_node();
    let id_node = root_node.covering_element(symbol_loc.range);
    let identifier_node = match id_node.kind() {
        SyntaxKind::Id => id_node.parent()?,
        SyntaxKind::Identifier => id_node.into_node()?,
        _ => return None,
    };

    let class_node = identifier_node.parent()?;
    let arg_list = match class_node.kind() {
        SyntaxKind::ClassRef => {
            let class_ref = ast::ClassRef::cast(class_node)?;
            class_ref.arg_value_list()?
        }
        SyntaxKind::ClassValue => {
            let class_value = ast::ClassValue::cast(class_node)?;
            class_value.arg_value_list()?
        }
        _ => return None,
    };

    let arg_ranges = arg_list
        .arg_values()
        .take_while(|it| matches!(it, ast::ArgValue::PositionalArgValue(_)))
        .map(|value| value.syntax().text_range());

    let template_arg_names = class
        .iter_template_arg()
        .map(|arg_id| symbol_map.template_arg(arg_id))
        .map(|arg| arg.name.clone());

    let mut hints = vec![];
    for (arg_range, name) in arg_ranges.zip(template_arg_names) {
        hints.push(InlayHint::new(
            arg_range.start(),
            format!("{}:", name),
            InlayHintKind::TemplateArg,
        ));
    }

    Some(hints)
}

fn inlay_hint_record_field(
    db: &dyn IndexDatabase,
    record_field: &RecordField,
    symbol_loc: FileRange,
) -> Option<Vec<InlayHint>> {
    let parse = db.parse(symbol_loc.file);
    let root_node = parse.syntax_node();
    let id_node = root_node.covering_element(symbol_loc.range);
    let identifier_node = match id_node.kind() {
        SyntaxKind::Id => id_node.parent()?,
        _ => return None,
    };

    let maybe_field_let_node = identifier_node.parent()?;
    if maybe_field_let_node.kind() != SyntaxKind::FieldLet {
        return None;
    }

    Some(vec![InlayHint::new(
        symbol_loc.range.end(),
        format!(":{}", record_field.typ),
        InlayHintKind::FieldLet,
    )])
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::InlayHint;

    fn check(s: &str) -> Vec<InlayHint> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.full_range(f.root_file())).expect("inlay hint not found")
    }

    #[test]
    fn template_arg() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo<int foo>;
class Bar: Foo<1>;"#
        ));
    }

    #[test]
    fn field_let() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo {
  int foo;
}
class Bar : Foo {
  let foo = 1;
}
"#
        ));
    }
}
