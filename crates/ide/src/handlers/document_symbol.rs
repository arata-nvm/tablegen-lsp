use ecow::{EcoString, eco_format};
use syntax::ast::{self, AstNode};
use syntax::parser::TextRange;

use crate::db::SourceDatabase;
use crate::file_system::FileId;

pub fn exec(db: &dyn SourceDatabase, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
    let parse = db.parse(file_id);
    let source_file = ast::SourceFile::cast(parse.syntax_node())?;
    let statement_list = source_file.statement_list()?;

    let mut symbols = Vec::new();
    for statement in statement_list.statements() {
        if let Some(document_symbol) = statement_to_document_symbol(&statement) {
            symbols.push(document_symbol);
        }
    }
    Some(symbols)
}

fn statement_to_document_symbol(statement: &ast::Statement) -> Option<DocumentSymbol> {
    match statement {
        ast::Statement::Class(class) => {
            let name = class.name()?.value()?;
            let range = class.syntax().text_range();

            let mut children = Vec::new();
            if let Some(body) = class.record_body().and_then(|it| it.body()) {
                for item in body.items() {
                    if let Some(child_symbol) = body_item_to_document_symbol(&item) {
                        children.push(child_symbol);
                    }
                }
            }

            Some(DocumentSymbol {
                name: eco_format!("class {}", name),
                typ: None,
                range,
                kind: DocumentSymbolKind::Class,
                children,
            })
        }
        ast::Statement::Def(def) => {
            let name = node_to_string(def.name()?);
            let range = def.syntax().text_range();

            let mut children = Vec::new();
            if let Some(body) = def.record_body().and_then(|it| it.body()) {
                for item in body.items() {
                    if let Some(child_symbol) = body_item_to_document_symbol(&item) {
                        children.push(child_symbol);
                    }
                }
            }

            Some(DocumentSymbol {
                name: eco_format!("def {name}"),
                typ: None,
                range,
                kind: DocumentSymbolKind::Def,
                children,
            })
        }
        ast::Statement::Defset(defset) => {
            let name = node_to_string(defset.name()?);
            let range = defset.syntax().text_range();

            let mut children = Vec::new();

            if let Some(statement_list) = defset.statement_list() {
                for stmt in statement_list.statements() {
                    if let Some(child_symbol) = statement_to_document_symbol(&stmt) {
                        children.push(child_symbol);
                    }
                }
            }

            Some(DocumentSymbol {
                name: eco_format!("defset {name}"),
                typ: None,
                range,
                kind: DocumentSymbolKind::Defset,
                children,
            })
        }
        ast::Statement::MultiClass(multiclass) => {
            let name = multiclass.name()?.value()?;
            let range = multiclass.syntax().text_range();

            let mut children = Vec::new();

            if let Some(statement_list) = multiclass.statement_list() {
                for stmt in statement_list.statements() {
                    if let Some(child_symbol) = statement_to_document_symbol(&stmt) {
                        children.push(child_symbol);
                    }
                }
            }

            Some(DocumentSymbol {
                name: eco_format!("multiclass {}", name),
                typ: None,
                range,
                kind: DocumentSymbolKind::Multiclass,
                children,
            })
        }
        ast::Statement::Defm(defm) => {
            let name = node_to_string(defm.name()?);
            let range = defm.syntax().text_range();

            Some(DocumentSymbol {
                name: eco_format!("defm {name}"),
                typ: None,
                range,
                kind: DocumentSymbolKind::Defm,
                children: vec![],
            })
        }
        ast::Statement::Defvar(defvar) => {
            let name = defvar.name()?.value()?;
            let range = defvar.syntax().text_range();

            Some(DocumentSymbol {
                name: eco_format!("defvar {name}"),
                typ: None,
                range,
                kind: DocumentSymbolKind::Variable,
                children: vec![],
            })
        }
        ast::Statement::Let(let_stmt) => {
            let let_list = let_stmt.let_list()?.syntax().text();
            let range = let_stmt.syntax().text_range();

            let mut children = Vec::new();

            if let Some(statement_list) = let_stmt.statement_list() {
                for stmt in statement_list.statements() {
                    if let Some(child_symbol) = statement_to_document_symbol(&stmt) {
                        children.push(child_symbol);
                    }
                }
            }

            Some(DocumentSymbol {
                name: eco_format!("let {}", let_list),
                typ: None,
                range,
                kind: DocumentSymbolKind::Let,
                children,
            })
        }
        _ => None,
    }
}

fn body_item_to_document_symbol(item: &ast::BodyItem) -> Option<DocumentSymbol> {
    match item {
        ast::BodyItem::FieldDef(field) => {
            let name = field.name()?.value()?;
            let typ = field.r#type()?;
            Some(DocumentSymbol {
                name,
                typ: Some(type_to_string(&typ)),
                range: field.syntax().text_range(),
                kind: DocumentSymbolKind::Field,
                children: Vec::new(),
            })
        }
        ast::BodyItem::FieldLet(field) => {
            let name = field.name()?.value()?;
            Some(DocumentSymbol {
                name,
                typ: None,
                range: field.syntax().text_range(),
                kind: DocumentSymbolKind::Field,
                children: Vec::new(),
            })
        }
        ast::BodyItem::Defvar(defvar) => {
            let name = defvar.name()?.value()?;
            Some(DocumentSymbol {
                name: eco_format!("defvar {name}"),
                typ: None,
                range: defvar.syntax().text_range(),
                kind: DocumentSymbolKind::Variable,
                children: Vec::new(),
            })
        }
        _ => None,
    }
}

fn type_to_string(typ: &ast::Type) -> EcoString {
    match typ {
        ast::Type::BitType(_) => "bit".into(),
        ast::Type::IntType(_) => "int".into(),
        ast::Type::StringType(_) => "string".into(),
        ast::Type::DagType(_) => "dag".into(),
        ast::Type::CodeType(_) => "code".into(),
        ast::Type::BitsType(bits) => {
            if let Some(length) = bits.length()
                && let Some(value) = length.value()
            {
                return format!("bits<{}>", value).into();
            }
            "bits".into()
        }
        ast::Type::ListType(list) => {
            if let Some(inner) = list.inner_type() {
                return format!("list<{}>", type_to_string(&inner)).into();
            }
            "list".into()
        }
        ast::Type::ClassId(class_id) => {
            if let Some(name) = class_id.name()
                && let Some(name_str) = name.value()
            {
                return name_str;
            }
            "?".into()
        }
    }
}

fn node_to_string(node: impl ast::AstNode) -> EcoString {
    node.syntax().text().to_string().trim().into()
}

#[derive(Debug)]
pub struct DocumentSymbol {
    pub name: EcoString,
    pub typ: Option<EcoString>,
    pub range: TextRange,
    pub kind: DocumentSymbolKind,
    pub children: Vec<DocumentSymbol>,
}

#[derive(Debug)]
pub enum DocumentSymbolKind {
    Class,
    Field,
    Def,
    Variable,
    Defset,
    Multiclass,
    Defm,
    Let,
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::DocumentSymbol;

    fn check(s: &str) -> Option<Vec<DocumentSymbol>> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.root_file())
    }

    #[test]
    fn single_file() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo<int size> {
    int field;
}

class Bar;
"#,
        ));
    }

    #[test]
    fn multiple_files() {
        let (db, f) = tests::multiple_files(
            r#"
; main.td
include "sub.td"
class Foo;

; sub.td
class Bar;
            "#,
        );
        let symbols = super::exec(&db, f.root_file());
        insta::assert_debug_snapshot!(symbols);
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(check(
            r#"
class Bar;
class Foo {
    bit a;
    int b;
    string c;
    dag d;
    bits<4> e;
    list<int> f;
    Bar g;
    code h;
}
            "#,
        ));
    }

    #[test]
    fn recursive_class() {
        insta::assert_debug_snapshot!(check(r#" class Foo { Foo g; } "#));
    }

    #[test]
    fn def() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo;
def foo : Foo {
  int a
};
"#
        ));
    }

    #[test]
    fn defset() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo;
defset list<Foo> foos = {
    def foo: Foo;
}
"#
        ));
    }

    #[test]
    fn defvar() {
        insta::assert_debug_snapshot!(check(
            r#"
defvar foo = 1;
"#
        ));
    }

    #[test]
    fn multiclass() {
        insta::assert_debug_snapshot!(check(
            r#"
multiclass Foo {
    def a;
}
defm foo: Foo;
"#
        ));
    }
}
