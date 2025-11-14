use crate::symbol_map::{SymbolMap, symbol::Symbol};
use std::fmt::Write;

pub fn dump_symbol_map(symbol_map: &SymbolMap) -> String {
    let mut output = String::new();

    for file_id in symbol_map.iter_file() {
        let Some(symbol_id_iter) = symbol_map.iter_symbols_in_file(file_id) else {
            continue;
        };

        let mut symbol_ids = symbol_id_iter.collect::<Vec<_>>();
        symbol_ids.sort_by_key(|id| symbol_map.symbol(*id).define_loc().range.start());

        for symbol_id in symbol_ids {
            let symbol = symbol_map.symbol(symbol_id);
            match symbol {
                Symbol::Class(class) => {
                    write!(output, "class {}<", class.name).unwrap();
                    for template_arg_id in class.iter_template_arg() {
                        let template_arg = symbol_map.template_arg(template_arg_id);
                        write!(output, "{} {}, ", template_arg.typ, template_arg.name).unwrap();
                    }
                    write!(output, "> : ").unwrap();
                    for parent_id in &class.parent_list {
                        let parent = symbol_map.class(*parent_id);
                        write!(output, "{}, ", parent.name).unwrap();
                    }
                    writeln!(output, "{{").unwrap();

                    for field_id in class.iter_field() {
                        let field = symbol_map.record_field(field_id);
                        writeln!(output, "  {} {};", field.typ, field.name).unwrap();
                    }

                    writeln!(output, "}}").unwrap();
                }
                Symbol::Def(def) => {
                    write!(output, "def {} : ", def.name).unwrap();
                    for parent_id in &def.parent_list {
                        let parent = symbol_map.class(*parent_id);
                        write!(output, "{}, ", parent.name).unwrap();
                    }
                    writeln!(output, "{{").unwrap();

                    for field_id in def.iter_field() {
                        let field = symbol_map.record_field(field_id);
                        writeln!(output, "  {} {};", field.typ, field.name).unwrap();
                    }

                    writeln!(output, "}}").unwrap();
                }
                Symbol::Multiclass(multiclass) => {
                    writeln!(output, "multiclass {}", multiclass.name).unwrap();
                }
                Symbol::Defset(defset) => {
                    writeln!(output, "defset {} {}", defset.typ, defset.name).unwrap();
                }
                Symbol::Variable(variable) => {
                    writeln!(output, "variable {} {}", variable.typ, variable.name,).unwrap();
                }
                _ => {}
            }
        }
    }

    output
}
