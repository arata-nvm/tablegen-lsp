use crate::eval::EvalDatabase;
use crate::file_system::FilePosition;
use crate::symbol_map::Symbol;

pub fn exec(db: &dyn EvalDatabase, pos: FilePosition) -> Option<Hover> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();
    let symbol = symbol_map.find_symbol_at(pos)?;

    let content = match symbol {
        Symbol::Class(class) => {
            let name = &class.name;
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
        Symbol::TemplateArgument(template_arg) => {
            format!("{} {}", template_arg.typ, template_arg.name)
        }
        Symbol::Field(field) => {
            let parent_class = symbol_map.class(field.parent);
            format!("{} {}::{}", field.typ, parent_class.name, field.name)
        }
    };

    Some(Hover { content })
}

#[derive(Debug)]
pub struct Hover {
    pub content: String,
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> String {
        let (db, f) = tests::single_file(s);
        let hover = super::exec(&db, f.marker(0)).expect("definition not found");
        hover.content
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
}
