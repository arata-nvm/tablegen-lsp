use crate::{
    kind::{SyntaxKind, TokenKind},
    parser::Parser,
    T,
};

use super::{delimited, r#type, value};

// StatementList ::= Statement*
pub(super) fn statement_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() {
        statement(p);
    }
    p.wrap(m, SyntaxKind::StatementList);
}

// Statement ::= Include | Assert | Class | Def | Defm | Defset | Defvar | Foreach | If | Let | MultiClass
pub(super) fn statement(p: &mut Parser) {
    match p.current() {
        T![include] => include(p),
        T![class] => class(p),
        T![def] => def(p),
        T![let] => r#let(p),
        _ => p.error_and_eat("expected class, def, defm, defset, multiclass, let or foreach"),
    }
}

// Include ::= "include" String
pub(super) fn include(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![include]);
    value::string(p).or_error(p, "expected filename after include");
    p.wrap(m, SyntaxKind::Include);
}

// Class ::= "class" Identifier TemplateArgList? RecordBody
pub(super) fn class(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![class]);
    value::identifier(p).or_error(p, "expected class name after 'class' keyword");
    opt_template_arg_list(p);
    record_body(p);
    p.wrap(m, SyntaxKind::Class);
}

// Def ::= "def" Value? RecordBody
pub(super) fn def(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![def]);
    value::value(p);
    record_body(p);
    p.wrap(m, SyntaxKind::Def);
}

// Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement );
pub(super) fn r#let(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![let]);
    let_list(p);
    p.expect_with_msg(T![in], "expected 'in' at end of top-level 'let'");
    if p.eat_if(T!['{']) {
        while !p.at(T!['}']) && !p.eof() {
            statement(p);
        }
        p.expect_with_msg(T!['}'], "expected '}' at end of top-level let command");
    } else {
        statement(p);
    }
    p.wrap(m, SyntaxKind::Let);
}

// LetList ::= LetItem ( "," LetItem )*
pub(super) fn let_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() {
        let_item(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::LetList);
}

// LetItem ::= Identifier ( "<" RangeList ">" )? "=" Value
pub(super) fn let_item(p: &mut Parser) {
    let m = p.marker();
    value::identifier(p).or_error(p, "expected identifier in let expression");
    p.expect_with_msg(T![=], "expected '=' in let expression");
    value::value(p);
    p.wrap(m, SyntaxKind::LetItem);
}

pub(super) fn opt_template_arg_list(p: &mut Parser) {
    if p.at(T![<]) {
        template_arg_list(p);
    }
}

// TemplateArgList ::= "<" TemplateArgDecl ( "," TemplateArgDecl )* ">"
pub(super) fn template_arg_list(p: &mut Parser) {
    let m = p.marker();
    delimited(p, T![<], T![>], T![,], template_arg_decl);
    p.wrap(m, SyntaxKind::TemplateArgList);
}

// TemplateArgDecl ::= Type Identifier ( "=" Value )?
pub(super) fn template_arg_decl(p: &mut Parser) {
    let m = p.marker();
    r#type::r#type(p);
    value::identifier(p).or_error(p, "expected identifier in declaration");
    if p.eat_if(T![=]) {
        value::value(p);
    }
    p.wrap(m, SyntaxKind::TemplateArgDecl);
}

// RecordBody ::= ParentClassList Body
pub(super) fn record_body(p: &mut Parser) {
    let m = p.marker();
    parent_class_list(p);
    body(p);
    p.wrap(m, SyntaxKind::RecordBody);
}

// ParentClassList ::= ( ":" ClassRef ( "," ClassRef )* )?
pub(super) fn parent_class_list(p: &mut Parser) {
    let m = p.marker();
    if !p.eat_if(T![:]) {
        p.wrap(m, SyntaxKind::ParentClassList);
        return;
    }

    while !p.eof() {
        class_ref(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::ParentClassList);
}

// ClassRef ::= Identifier ( "<" ArgValueList? ">" )?
pub(super) fn class_ref(p: &mut Parser) {
    let m = p.marker();
    value::identifier(p);
    if p.eat_if(T![<]) {
        arg_value_list(p);
        p.expect_with_msg(T![>], "expected '>' in template value list");
    }
    p.wrap(m, SyntaxKind::ClassRef);
}

// ArgValueList ::= PositionalArgValueList ","? NamedArgValueList
pub(super) fn arg_value_list(p: &mut Parser) {
    let m = p.marker();
    positional_arg_value_list(p);
    p.wrap(m, SyntaxKind::ArgValueList);
}

// PositionalArgValueList ::= ( Value ( "," Value )* ) ?
pub(super) fn positional_arg_value_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() {
        value::value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::PositionalArgValueList);
}

// Body ::= ";" | "{" BodyItem* "}"
pub(super) fn body(p: &mut Parser) {
    let m = p.marker();
    if p.eat_if(T![;]) {
        p.wrap(m, SyntaxKind::Body);
        return;
    }

    p.expect_with_msg(T!['{'], "expected ';' or '{' to start body");
    while !p.at(T!['}']) && !p.eof() {
        if !body_item(p) {
            break;
        }
    }
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::Body);
}

// BodyItem ::= FieldDef | FieldLet | Defvar | Assert
pub(super) fn body_item(p: &mut Parser) -> bool {
    match p.current() {
        T![let] => field_let(p),
        T![code] | TokenKind::Id => field_def(p),
        _ => return false,
    }
    true
}

// FieldDef ::= ( Type | CodeType ) Identifier ( "=" Value )? ";"
pub(super) fn field_def(p: &mut Parser) {
    let m = p.marker();
    if p.at(T![code]) {
        code_type(p);
    } else {
        r#type::r#type(p);
    }
    value::identifier(p).or_error(p, "expected identifier in declaration");
    if p.eat_if(T![=]) {
        value::value(p);
    }
    p.expect_with_msg(T![;], "expected ';' after declaration");
    p.wrap(m, SyntaxKind::FieldDef);
}

// CodeType ::= "code"
pub(super) fn code_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![code]);
    p.wrap(m, SyntaxKind::CodeType);
}

// FieldLet ::= "let" Identitfer ( "{" RangeList "}" )? "=" Value ";"
pub(super) fn field_let(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![let]);
    value::identifier(p).or_error(p, "expected field identifier after let");
    p.expect(T![=]);
    value::value(p).or_error(p, "expected '=' in let expression");
    p.expect_with_msg(T![;], "expected ';' after let expression");
    p.wrap(m, SyntaxKind::FieldLet);
}
