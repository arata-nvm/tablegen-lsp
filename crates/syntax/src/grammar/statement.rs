use crate::{
    grammar::{delimited, r#type, value},
    parser::Parser,
    syntax_kind::SyntaxKind,
    token_kind::TokenKind,
    T,
};

pub(super) enum StatementListType {
    TopLevel,
    Block,
    SingleOrBlock,
}

// StatementList ::= Statement*
pub(super) fn statement_list(p: &mut Parser, typ: StatementListType) {
    p.start_node(SyntaxKind::StatementList);
    p.skip();
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
    p.finish_node();
}

// Statement ::= Include | Assert | Class | Def | Defm | Defset | Defvar | Foreach | If | Let | MultiClass
pub(super) fn statement(p: &mut Parser) {
    match p.peek() {
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
    p.start_node(SyntaxKind::Include);
    p.assert(T![include]);
    value::string(p).or_error(p, "expected filename after include");
    p.finish_node();
}

// Class ::= "class" Identifier TemplateArgList? RecordBody
pub(super) fn class(p: &mut Parser) {
    p.start_node(SyntaxKind::Class);
    p.assert(T![class]);
    value::identifier(p).or_error(p, "expected class name after 'class' keyword");
    opt_template_arg_list(p);
    record_body(p);
    p.finish_node();
}

// Def ::= "def" Value? RecordBody
pub(super) fn def(p: &mut Parser) {
    p.start_node(SyntaxKind::Def);
    p.assert(T![def]);
    object_name(p);
    record_body(p);
    p.finish_node();
}

pub(super) fn object_name(p: &mut Parser) {
    match p.peek() {
        T![:] | T![;] | T!['{'] => {}
        _ => value::opt_name_value(p),
    }
}

// Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement );
pub(super) fn r#let(p: &mut Parser) {
    p.start_node(SyntaxKind::Let);
    p.assert(T![let]);
    let_list(p);
    p.expect_with_msg(T![in], "expected 'in' at end of top-level 'let'");
    statement_list(p, StatementListType::SingleOrBlock);
    p.finish_node();
}

// LetList ::= LetItem ( "," LetItem )*
pub(super) fn let_list(p: &mut Parser) {
    p.start_node(SyntaxKind::LetList);
    while !p.eof() {
        let_item(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.finish_node();
}

// LetItem ::= Identifier ( "<" RangeList ">" )? "=" Value
pub(super) fn let_item(p: &mut Parser) {
    p.start_node(SyntaxKind::LetItem);
    value::identifier(p).or_error(p, "expected identifier in let expression");
    if p.eat_if(T![<]) {
        value::range_list(p);
        p.expect_with_msg(T![>], "expected '>' at end of range list");
    }
    p.expect_with_msg(T![=], "expected '=' in let expression");
    value::value(p);
    p.finish_node();
}

// MultiClass ::= "multiclass" Identifier TemplateArgList? ParentClassList "{" MultiClassStatement+ "}"
pub(super) fn multi_class(p: &mut Parser) {
    p.start_node(SyntaxKind::MultiClass);
    p.assert(T![multiclass]);
    value::identifier(p).or_error(p, "expected identifier after multiclass for name");
    opt_template_arg_list(p);
    parent_class_list(p);
    p.expect_with_msg(T!['{'], "expected '{' in multiclass definition");
    multi_class_statements(p);
    p.finish_node();
}

pub(super) fn multi_class_statements(p: &mut Parser) {
    p.start_node(SyntaxKind::StatementList);
    multi_class_statement(p);
    while !p.at(T!['}']) && !p.eof() {
        multi_class_statement(p);
    }
    p.expect(T!['}']);
    p.finish_node();
}

// MultiClassStatement ::= Def | Defm | Foreach | Let | If
pub(super) fn multi_class_statement(p: &mut Parser) {
    match p.peek() {
        T![def] => def(p),
        T![defm] => defm(p),
        T![foreach] => foreach(p),
        T![let] => r#let(p),
        T![assert] => r#assert(p),
        T![if] => r#if(p),
        _ => p.error_and_eat("expected 'let', 'def', 'defm' or 'foreach' in multiclass body"),
    }
}

// Defm ::= "defm" Value? ParentClassList ";"
pub(super) fn defm(p: &mut Parser) {
    p.start_node(SyntaxKind::Defm);
    p.assert(T![defm]);
    object_name(p);
    parent_class_list(p);
    p.expect_with_msg(T![;], "expected ';' at end of defm");
    p.finish_node();
}

// Defset ::= "defset" Type Identifier "=" "{" Statement* "}"
pub(super) fn defset(p: &mut Parser) {
    p.start_node(SyntaxKind::Defset);
    p.assert(T![defset]);
    r#type::r#type(p);
    value::identifier(p);
    p.expect(T![=]);
    statement_list(p, StatementListType::Block);
    p.finish_node();
}

// Defvar ::= "defvar" Identifier "=" Value ";"
pub(super) fn defvar(p: &mut Parser) {
    p.start_node(SyntaxKind::Defvar);
    p.assert(T![defvar]);
    value::identifier(p);
    p.expect(T![=]);
    value::value(p);
    p.expect(T![;]);
    p.finish_node();
}

// Foreach ::= "foreach" ForeachIterator "in" ( "{" Statement* "}" | Statement )
pub(super) fn foreach(p: &mut Parser) {
    p.start_node(SyntaxKind::Foreach);
    p.assert(T![foreach]);
    foreach_iterator(p);
    p.expect(T![in]);
    statement_list(p, StatementListType::SingleOrBlock);
    p.finish_node();
}

// ForeachIterator ::= Identifier "=" ForeachIteratorInit
pub(super) fn foreach_iterator(p: &mut Parser) {
    p.start_node(SyntaxKind::ForeachIterator);
    value::identifier(p).or_error(p, "expected identifier in foreach declaration");
    p.expect_with_msg(T![=], "expected '=' in foreach declaration");
    foreach_iterator_init(p);
    p.finish_node()
}

// ForeachIteratorInit ::= "{" RangeList "}" | RangePiece | Value
pub(super) fn foreach_iterator_init(p: &mut Parser) {
    match p.peek() {
        T!['{'] => {
            p.assert(T!['{']);
            let m = value::range_list(p);
            p.expect_with_msg(T!['}'], "expected '}' at end of bit range list");
            m
        }
        TokenKind::IntVal => value::range_piece(p),
        _ => value::value(p),
    };
}

// If ::= "if" Value "then" ( "{" Statement* "}" | Statement ) ( "else" ( "{" Statement* "}" | Statement ) )?
fn r#if(p: &mut Parser) {
    p.start_node(SyntaxKind::If);
    p.assert(T![if]);
    value::value(p);
    p.expect(T![then]);
    statement_list(p, StatementListType::SingleOrBlock);
    if p.eat_if(T![else]) {
        statement_list(p, StatementListType::SingleOrBlock);
    }
    p.finish_node();
}

// Assert ::= "assert" Value "," Value ";"
fn r#assert(p: &mut Parser) {
    p.start_node(SyntaxKind::Assert);
    p.assert(T![assert]);
    value::value(p);
    p.expect(T![,]);
    value::value(p);
    p.expect(T![;]);
    p.finish_node();
}

pub(super) fn opt_template_arg_list(p: &mut Parser) {
    if p.at(T![<]) {
        template_arg_list(p);
    }
}

// TemplateArgList ::= "<" TemplateArgDecl ( "," TemplateArgDecl )* ">"
pub(super) fn template_arg_list(p: &mut Parser) {
    p.start_node(SyntaxKind::TemplateArgList);
    delimited(p, T![<], T![>], T![,], template_arg_decl);
    p.finish_node();
}

// TemplateArgDecl ::= Type Identifier ( "=" Value )?
pub(super) fn template_arg_decl(p: &mut Parser) {
    p.start_node(SyntaxKind::TemplateArgDecl);
    r#type::r#type(p);
    value::identifier(p).or_error(p, "expected identifier in declaration");
    if p.eat_if(T![=]) {
        value::value(p);
    }
    p.finish_node();
}

// RecordBody ::= ParentClassList Body
pub(super) fn record_body(p: &mut Parser) {
    p.start_node(SyntaxKind::RecordBody);
    parent_class_list(p);
    body(p);
    p.finish_node();
}

// ParentClassList ::= ( ":" ClassRef ( "," ClassRef )* )?
pub(super) fn parent_class_list(p: &mut Parser) {
    p.start_node(SyntaxKind::ParentClassList);
    if !p.eat_if(T![:]) {
        p.finish_node();
        return;
    }

    while !p.eof() {
        class_ref(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.finish_node();
}

// ClassRef ::= Identifier ( "<" ArgValueList? ">" )?
pub(super) fn class_ref(p: &mut Parser) {
    p.start_node(SyntaxKind::ClassRef);
    value::identifier(p);
    if p.eat_if(T![<]) {
        arg_value_list(p);
        p.expect_with_msg(T![>], "expected '>' in template value list");
    }
    p.finish_node();
}

// ArgValueList ::= PositionalArgValueList ","? NamedArgValueList
pub(super) fn arg_value_list(p: &mut Parser) {
    p.start_node(SyntaxKind::ArgValueList);
    if p.at_set(&value::VALUE_START) {
        positional_arg_value_list(p);
    }
    // TODO
    // p.eat_if(T![,]);
    // if p.at_set(&VALUE_START) {
    // named_arg_value_list(p);
    // }
    p.finish_node();
}

// PositionalArgValueList ::= ( Value ( "," Value )* ) ?
pub(super) fn positional_arg_value_list(p: &mut Parser) {
    p.start_node(SyntaxKind::PositionalArgValueList);
    while !p.eof() {
        value::value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.finish_node();
}

// NamedArgValueList ::= ( NamedArgValue ( "," NamedArgValue )* )?
#[allow(unused)]
pub(super) fn named_arg_value_list(p: &mut Parser) {
    p.start_node(SyntaxKind::NamedArgValueList);
    while !p.eof() {
        named_arg_value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.finish_node();
}

// NamedArgValue ::= Value "=" Value
#[allow(unused)]
pub(super) fn named_arg_value(p: &mut Parser) {
    p.start_node(SyntaxKind::NamedArgValue);
    value::value(p);
    p.expect(T![=]);
    value::value(p);
    p.finish_node();
}

// Body ::= ";" | "{" BodyItem* "}"
pub(super) fn body(p: &mut Parser) {
    p.start_node(SyntaxKind::Body);
    if p.eat_if(T![;]) {
        p.finish_node();
        return;
    }

    p.expect_with_msg(T!['{'], "expected ';' or '{' to start body");
    while !p.at(T!['}']) && !p.eof() {
        if !body_item(p) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_node();
}

// BodyItem ::= FieldDef | FieldLet | Defvar | Assert
pub(super) fn body_item(p: &mut Parser) -> bool {
    if p.at_set(&r#type::TYPE_FIRST_TOKENS) || p.at(T![field]) {
        field_def(p);
        return true;
    }

    match p.peek() {
        T![let] => field_let(p),
        T![defvar] => defvar(p),
        T![assert] => r#assert(p),
        _ => return false,
    }

    true
}

// FieldDef ::= "field"? ( Type | CodeType ) Identifier ( "=" Value )? ";"
pub(super) fn field_def(p: &mut Parser) {
    p.start_node(SyntaxKind::FieldDef);
    p.eat_if(T![field]);
    r#type::r#type(p);
    value::identifier(p).or_error(p, "expected identifier in declaration");
    if p.eat_if(T![=]) {
        value::value(p);
    }
    p.expect_with_msg(T![;], "expected ';' after declaration");
    p.finish_node();
}

// FieldLet ::= "let" Identitfer ( "{" RangeList "}" )? "=" Value ";"
pub(super) fn field_let(p: &mut Parser) {
    p.start_node(SyntaxKind::FieldLet);
    p.assert(T![let]);
    value::identifier(p).or_error(p, "expected field identifier after let");
    if p.eat_if(T!['{']) {
        value::range_list(p);
        p.expect_with_msg(T!['}'], "expected '}' at end of bit list");
    }
    p.expect(T![=]);
    value::value(p).or_error(p, "expected '=' in let expression");
    p.expect_with_msg(T![;], "expected ';' after let expression");
    p.finish_node();
}
