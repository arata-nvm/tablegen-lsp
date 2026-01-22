use crate::file_system::{FilePosition, FileRange, SourceUnitId};
use crate::index::IndexDatabase;
use crate::symbol_map::symbol::Symbol;
use crate::symbol_map::variable::VariableKind;
use crate::symbol_map::{SymbolMap, record::AsRecordData};
use crate::utils;

#[derive(Debug)]
pub struct Hover {
    pub signature: String,
    pub document: Option<String>,
}

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    pos: FilePosition,
) -> Option<Hover> {
    let index = db.index(source_unit_id);
    let symbol_map = index.symbol_map();

    let (signature, define_loc) = extract_symbol_signature(symbol_map, pos)?;

    let parse = db.parse(define_loc.file);
    let symbol_doc = utils::extract_doc_comments(parse.syntax_node(), define_loc.range);

    Some(Hover {
        signature,
        document: symbol_doc,
    })
}

fn extract_symbol_signature(
    symbol_map: &SymbolMap,
    pos: FilePosition,
) -> Option<(String, FileRange)> {
    let symbol_id = symbol_map.find_symbol_at(pos)?;
    let symbol = symbol_map.symbol(symbol_id);

    let symbol_info = match symbol {
        Symbol::Class(class) => {
            let name = class.name();
            let template_arg = class
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
        Symbol::Def(def) => {
            format!("def {}", def.name())
        }
        Symbol::TemplateArgument(template_arg) => {
            format!("{} {}", template_arg.typ, template_arg.name)
        }
        Symbol::RecordField(record_field) => {
            let parent = symbol_map.record(record_field.parent);
            format!(
                "{} {}::{}",
                record_field.typ,
                parent.name(),
                record_field.name
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

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::Hover;

    fn check(s: &str) -> Hover {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.source_unit_id(), f.marker(0)).expect("definition not found")
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
