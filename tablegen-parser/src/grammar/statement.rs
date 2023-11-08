use crate::{
    kind::{SyntaxKind, TokenKind},
    parser::Parser,
    T,
};

use super::{delimited, r#type, value};

pub(super) enum StatementListType {
    TopLevel,
    Block,
    SingleOrBlock,
}

// StatementList ::= Statement*
pub(super) fn statement_list(p: &mut Parser, typ: StatementListType) {
    let m = p.marker();
    match typ {
        StatementListType::TopLevel => {
            while !p.eof() {
                statement(p);
            }
        }
        StatementListType::Block => {
            p.expect(T!['{']);
            while !p.at(T!['}']) && !p.eof() {
                statement(p);
            }
            p.expect(T!['}']);
        }
        StatementListType::SingleOrBlock => {
            if p.eat_if(T!['{']) {
                while !p.at(T!['}']) && !p.eof() {
                    statement(p);
                }
                p.expect(T!['}']);
            } else {
                statement(p);
            }
        }
    }
    p.wrap(m, SyntaxKind::StatementList);
}

// Statement ::= Include | Assert | Class | Def | Defm | Defset | Defvar | Foreach | If | Let | MultiClass
pub(super) fn statement(p: &mut Parser) {
    match p.current() {
        T![include] => include(p),
        T![assert] => r#assert(p),
        T![class] => class(p),
        T![def] => def(p),
        T![defm] => defm(p),
        T![defset] => defset(p),
        T![defvar] => defvar(p),
        T![foreach] => foreach(p),
        T![if] => r#if(p),
        T![let] => r#let(p),
        T![multiclass] => multi_class(p),
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
    object_name(p);
    record_body(p);
    p.wrap(m, SyntaxKind::Def);
}

pub(super) fn object_name(p: &mut Parser) {
    match p.current() {
        T![:] | T![;] | T!['{'] => {}
        _ => value::opt_value(p),
    }
}

// Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement );
pub(super) fn r#let(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![let]);
    let_list(p);
    p.expect_with_msg(T![in], "expected 'in' at end of top-level 'let'");
    statement_list(p, StatementListType::SingleOrBlock);
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
    if p.eat_if(T![<]) {
        value::range_list(p);
        p.expect_with_msg(T![>], "expected '>' at end of range list");
    }
    p.expect_with_msg(T![=], "expected '=' in let expression");
    value::value(p);
    p.wrap(m, SyntaxKind::LetItem);
}

// MultiClass ::= "multiclass" Identifier TemplateArgList? ParentClassList "{" MultiClassStatement+ "}"
pub(super) fn multi_class(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![multiclass]);
    value::identifier(p).or_error(p, "expected identifier after multiclass for name");
    opt_template_arg_list(p);
    parent_class_list(p);
    p.expect_with_msg(T!['{'], "expected '{' in multiclass definition");
    multi_class_statements(p);
    p.wrap(m, SyntaxKind::MultiClass);
}

pub(super) fn multi_class_statements(p: &mut Parser) {
    let m = p.marker();
    multi_class_statement(p);
    while !p.at(T!['}']) && !p.eof() {
        multi_class_statement(p);
    }
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::StatementList);
}

// MultiClassStatement ::= Def | Defm | Foreach | Let
pub(super) fn multi_class_statement(p: &mut Parser) {
    match p.current() {
        T![def] => def(p),
        T![defm] => defm(p),
        T![foreach] => foreach(p),
        T![let] => r#let(p),
        _ => p.error_and_eat("expected 'let', 'def', 'defm' or 'foreach' in multiclass body"),
    }
}

// Defm ::= "defm" Value? ParentClassList ";"
pub(super) fn defm(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![defm]);
    value::opt_value(p);
    parent_class_list(p);
    p.expect_with_msg(T![;], "expected ';' at end of defm");
    p.wrap(m, SyntaxKind::Defm);
}

// Defset ::= "defset" Type Identifier "=" "{" Statement* "}"
pub(super) fn defset(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![defset]);
    r#type::r#type(p);
    value::identifier(p);
    p.expect(T![=]);
    statement_list(p, StatementListType::Block);
    p.wrap(m, SyntaxKind::Defset);
}

// Defvar ::= "defvar" Identifier "=" Value ";"
pub(super) fn defvar(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![defvar]);
    value::identifier(p);
    p.expect(T![=]);
    value::value(p);
    p.expect(T![;]);
    p.wrap(m, SyntaxKind::Defvar);
}

// Foreach ::= "foreach" ForeachIterator "in" ( "{" Statement* "}" | Statement )
pub(super) fn foreach(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![foreach]);
    foreach_iterator(p);
    p.expect(T![in]);
    statement_list(p, StatementListType::SingleOrBlock);
    p.wrap(m, SyntaxKind::Foreach);
}

// ForeachIterator ::= Identifier "=" ForeachIteratorInit
pub(super) fn foreach_iterator(p: &mut Parser) {
    let m = p.marker();
    value::identifier(p).or_error(p, "expected identifier in foreach declaration");
    p.expect_with_msg(T![=], "expected '=' in foreach declaration");
    foreach_iterator_init(p);
    p.wrap(m, SyntaxKind::ForeachIterator);
}

// ForeachIteratorInit ::=  "{" RangeList "}" | RangePiece | Value
pub(super) fn foreach_iterator_init(p: &mut Parser) {
    let m = p.marker();
    match p.current() {
        T!['{'] => {
            p.assert(T!['{']);
            value::range_list(p);
            p.expect(T!['}']);
        }
        TokenKind::IntVal => {
            value::range_piece(p);
        }
        _ => {
            value::value(p);
        }
    }
    p.wrap(m, SyntaxKind::ForeachIteratorInit);
}

// If ::= "if" Value "then" ( "{" Statement* "}" | Statement )
fn r#if(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![if]);
    value::value(p);
    p.expect(T![then]);
    statement_list(p, StatementListType::SingleOrBlock);
    p.wrap(m, SyntaxKind::If);
}

// Assert ::= "assert" Value "," Value ";"
fn r#assert(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![assert]);
    value::value(p);
    p.expect(T![,]);
    value::value(p);
    p.expect(T![;]);
    p.wrap(m, SyntaxKind::Assert);
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
    if p.at_set(&value::VALUE_START) {
        positional_arg_value_list(p);
    }
    // TODO
    // p.eat_if(T![,]);
    // if p.at_set(&VALUE_START) {
    // named_arg_value_list(p);
    // }
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

// NamedArgValueList ::= ( NamedArgValue ( "," NamedArgValue )* )?
pub(super) fn named_arg_value_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() {
        named_arg_value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::NamedArgValueList);
}

// NamedArgValue ::= Value "=" Value
pub(super) fn named_arg_value(p: &mut Parser) {
    let m = p.marker();
    value::value(p);
    p.expect(T![=]);
    value::value(p);
    p.wrap(m, SyntaxKind::NamedArgValue);
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
    if p.at_set(&r#type::TYPE_FIRST_TOKENS) || p.at(T![field]) {
        field_def(p);
        return true;
    }

    match p.current() {
        T![let] => field_let(p),
        T![defvar] => defvar(p),
        T![assert] => r#assert(p),
        _ => return false,
    }

    true
}

// FieldDef ::= "field"? ( Type | CodeType ) Identifier ( "=" Value )? ";"
pub(super) fn field_def(p: &mut Parser) {
    let m = p.marker();
    p.eat_if(T![field]);
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
    if p.eat_if(T!['{']) {
        value::range_list(p);
        p.expect_with_msg(T!['}'], "expected '}' at end of bit list");
    }
    p.expect(T![=]);
    value::value(p).or_error(p, "expected '=' in let expression");
    p.expect_with_msg(T![;], "expected ';' after let expression");
    p.wrap(m, SyntaxKind::FieldLet);
}
