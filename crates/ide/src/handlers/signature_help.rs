use syntax::ast::{self, AstNode};

use crate::{
    bang_operator::BangOperatorMetadata,
    db::IndexDatabase,
    file_system::{FilePosition, SourceUnitId},
    index::Index,
    symbol_map::{SymbolMap, record::AsRecordData, template_arg::TemplateArgumentId},
    utils::{self, SyntaxNodeExt},
};

#[derive(Debug)]
pub struct SignatureHelp {
    pub label: String,
    pub document: Option<String>,
    pub parameters: Vec<SignatureParameter>,
    pub active_parameter: Option<usize>,
}

#[derive(Debug)]
pub struct SignatureParameter {
    pub label_start: u32,
    pub label_end: u32,
}

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    pos: FilePosition,
) -> Option<SignatureHelp> {
    let parse = db.parse(pos.file);
    let root_node = parse.syntax_node();
    let token_at_pos = root_node.token_at_offset(pos.position).left_biased()?;
    let node_at_pos = token_at_pos.parent()?;

    let Index { symbol_map, .. } = &*db.index(source_unit_id);

    if let Some(class_ref) = node_at_pos.ancestor::<ast::ClassRef>() {
        let class_name = class_ref.name()?.value()?;
        let class_id = symbol_map.find_class(&class_name)?;
        let class = symbol_map.class(class_id);
        let template_args: Vec<_> = class.iter_template_arg().collect();
        let active = find_active_parameter_in_arg_value_list(class_ref.arg_value_list()?, pos);
        let document = extract_class_doc(db, class);
        return build_class_signature_help(
            symbol_map,
            &class_name,
            &template_args,
            active,
            document,
        );
    }

    if let Some(class_value) = node_at_pos.ancestor::<ast::ClassValue>() {
        let class_name = class_value.name()?.value()?;
        let class_id = symbol_map.find_class(&class_name)?;
        let class = symbol_map.class(class_id);
        let template_args: Vec<_> = class.iter_template_arg().collect();
        let active = find_active_parameter_in_arg_value_list(class_value.arg_value_list()?, pos);
        let document = extract_class_doc(db, class);
        return build_class_signature_help(
            symbol_map,
            &class_name,
            &template_args,
            active,
            document,
        );
    }

    if let Some(bang_op) = node_at_pos.ancestor::<ast::BangOperator>() {
        let op_kind = bang_op.kind()?;
        let metadata = crate::bang_operator::get_metadata_for_syntax_kind(op_kind)?;
        let active = find_active_parameter_in_values(bang_op.values(), pos);
        return Some(build_bang_operator_signature_help(metadata, active));
    }

    None
}

fn find_active_parameter_in_arg_value_list(
    arg_value_list: ast::ArgValueList,
    pos: FilePosition,
) -> Option<usize> {
    let mut index = 0;
    for arg in arg_value_list.arg_values() {
        let range = arg.syntax().text_range();
        if pos.position <= range.end() {
            return Some(index);
        }
        index += 1;
    }
    Some(index)
}

fn find_active_parameter_in_values(
    values: impl Iterator<Item = ast::Value>,
    pos: FilePosition,
) -> Option<usize> {
    let mut index = 0;
    let mut found_any = false;
    for value in values {
        found_any = true;
        let range = value.syntax().text_range();
        if pos.position <= range.end() {
            return Some(index);
        }
        index += 1;
    }
    if found_any { Some(index) } else { Some(0) }
}

fn extract_class_doc(
    db: &dyn IndexDatabase,
    class: &crate::symbol_map::class::Class,
) -> Option<String> {
    let define_loc = class.define_loc();
    let parse = db.parse(define_loc.file);
    utils::extract_doc_comments(parse.syntax_node(), define_loc.range)
}

fn build_class_signature_help(
    symbol_map: &SymbolMap,
    name: &str,
    template_args: &[TemplateArgumentId],
    active_parameter: Option<usize>,
    document: Option<String>,
) -> Option<SignatureHelp> {
    if template_args.is_empty() {
        return None;
    }

    let mut label = format!("{name}<");
    let mut parameters = Vec::new();

    for (i, &arg_id) in template_args.iter().enumerate() {
        if i > 0 {
            label.push_str(", ");
        }
        let arg = symbol_map.template_arg(arg_id);
        let start = label.chars().count() as u32;
        label.push_str(&format!("{} {}", arg.typ, arg.name));
        if arg.has_default_value {
            label.push_str(" = ...");
        }
        let end = label.chars().count() as u32;
        parameters.push(SignatureParameter {
            label_start: start,
            label_end: end,
        });
    }

    label.push('>');

    Some(SignatureHelp {
        label,
        document,
        parameters,
        active_parameter,
    })
}

fn build_bang_operator_signature_help(
    metadata: &BangOperatorMetadata,
    active_parameter: Option<usize>,
) -> SignatureHelp {
    let parameters = metadata
        .signature_params
        .iter()
        .map(|param| SignatureParameter {
            label_start: param.label_start as u32,
            label_end: param.label_end as u32,
        })
        .collect();

    SignatureHelp {
        label: metadata.signature.to_string(),
        document: Some(metadata.documentation.to_string()),
        parameters,
        active_parameter,
    }
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> String {
        let (db, f) = tests::single_file(s);
        let pos = f.marker(0);
        let Some(sig_help) = super::exec(&db, f.source_unit_id(), pos) else {
            return "".into();
        };

        let mut out = String::new();
        out.push_str(&format!("{}\n", sig_help.label));
        let mut offset = 0;
        for (idx, param) in sig_help.parameters.iter().enumerate() {
            let is_active = sig_help.active_parameter == Some(idx);
            let marker = if is_active { "^" } else { "-" };

            let start = param.label_start as usize;
            let end = param.label_end as usize;

            out.push_str(&" ".repeat(start - offset));
            out.push_str(&marker.repeat(end - start));

            assert!(offset <= end);
            offset = end;
        }
        if let Some(document) = sig_help.document {
            out.push_str(&format!("\n------\n{document}"));
        }
        out
    }

    #[test]
    fn basic() {
        insta::assert_snapshot!(check(
            r#"
class Foo<int a, string b>;
class Bar: Foo<$1, "hello">;
"#
        ));
    }

    #[test]
    fn second_param() {
        insta::assert_snapshot!(check(
            r#"
class Foo<int a, string b>;
class Bar: Foo<1, $"hello">;
"#
        ));
    }

    #[test]
    fn after_comma() {
        insta::assert_snapshot!(check(
            r#"
class Foo<int a, string b>;
class Bar: Foo<1, $>;
"#
        ));
    }

    #[test]
    fn class_value() {
        insta::assert_snapshot!(check(
            r#"
class Foo<int a>;
def bar {
  Foo x = Foo<$1>;
}
"#
        ));
    }

    #[test]
    fn no_template_args() {
        insta::assert_snapshot!(check(
            r#"
class Foo;
class Bar: Foo<$>;
"#
        ));
    }

    #[test]
    fn default_value() {
        insta::assert_snapshot!(check(
            r#"
class Foo<int a, int b = 0>;
class Bar: Foo<$1>;
"#
        ));
    }

    #[test]
    fn bang_operator_basic() {
        insta::assert_snapshot!(check(
            r#"
class Foo {
  int x = !add($1, 2);
}
"#
        ));
    }

    #[test]
    fn bang_operator_second_param() {
        insta::assert_snapshot!(check(
            r#"
class Foo {
  int x = !add(1, $2);
}
"#
        ));
    }

    #[test]
    fn bang_operator_if() {
        insta::assert_snapshot!(check(
            r#"
class Foo {
  int x = !if($1, 2, 3);
}
"#
        ));
    }

    #[test]
    fn bang_operator_strconcat() {
        insta::assert_snapshot!(check(
            r#"
class Foo {
  string x = !strconcat($"a", "b");
}
"#
        ));
    }

    #[test]
    fn bang_operator_substr() {
        insta::assert_snapshot!(check(
            r#"
class Foo {
  string x = !substr($"hello", 1, 3);
}
"#
        ));
    }

    #[test]
    fn class_with_doc_comment() {
        insta::assert_snapshot!(check(
            r#"
// A base class for instructions.
// It has template arguments.
class Inst<int opcode, string mnemonic>;
class Add: Inst<$1, "add">;
"#
        ));
    }
}
