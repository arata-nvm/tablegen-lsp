use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    parser::Parser,
    T,
};

pub fn parse(text: &str) -> SyntaxNode {
    let mut parser = Parser::new(text);
    file(&mut parser);
    parser.finish().into_iter().next().unwrap()
}

// File ::= StatementList
fn file(p: &mut Parser) {
    let m = p.marker();
    p.eat_trivia();
    statement_list(p);
    p.wrap_all(m, SyntaxKind::File);
}

// StatementList ::= Statement*
fn statement_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() {
        statement(p);
    }
    p.wrap(m, SyntaxKind::StatementList);
}

// Statement ::= Include | Assert | Class | Def | Defm | Defset | Defvar | Foreach | If | Let | MultiClass
fn statement(p: &mut Parser) {
    match p.current() {
        T![include] => include(p),
        T![class] => class(p),
        T![def] => def(p),
        T![let] => r#let(p),
        _ => p.error_and_eat("Expected class, def, defm, defset, multiclass, let or foreach"),
    }
}

// Include ::= "include" String
fn include(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![include]);
    string(p);
    p.wrap(m, SyntaxKind::Include);
}

// Class ::= "class" Identifier TemplateArgList? RecordBody
fn class(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![class]);
    identifier(p);
    if p.at(T![<]) {
        template_arg_list(p);
    }
    record_body(p);
    p.wrap(m, SyntaxKind::Class);
}

// Def ::= "def" Value? RecordBody
fn def(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![def]);
    value(p);
    record_body(p);
    p.wrap(m, SyntaxKind::Def);
}

// Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement )
fn r#let(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![let]);
    let_list(p);
    p.expect(T![in]);
    if p.eat_if(T!['{']) {
        while !p.eof() && !p.at(T!['}']) {
            statement(p);
        }
        p.expect(T!['}']);
    } else {
        statement(p);
    }
    p.wrap_all(m, SyntaxKind::Let);
}

// LetList ::= LetItem ( "," LetItem )*
fn let_list(p: &mut Parser) {
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
fn let_item(p: &mut Parser) {
    let m = p.marker();
    identifier(p);
    p.expect(T![=]);
    value(p);
    p.wrap(m, SyntaxKind::LetItem);
}

// TemplateArgList ::= "<" TemplateArgDecl ( "," TemplateArgDecl )* ">"
fn template_arg_list(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![<]);
    while !p.eof() && !p.at(T![>]) {
        tempalte_arg_decl(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T![>]);
    p.wrap(m, SyntaxKind::TemplateArgList);
}

// TemplateArgDecl ::= Type Identifier ( "=" Value )?
fn tempalte_arg_decl(p: &mut Parser) {
    let m = p.marker();
    r#type(p);
    identifier(p);
    if p.eat_if(T![=]) {
        value(p);
    }
    p.wrap(m, SyntaxKind::TemplateArgDecl);
}

// RecordBody ::= ParentClassList Body
fn record_body(p: &mut Parser) {
    let m = p.marker();
    parent_class_list(p);
    body(p);
    p.wrap(m, SyntaxKind::RecordBody);
}

// ParentClassList ::= ( ":" ClassRef ( "," ClassRef )* )?
fn parent_class_list(p: &mut Parser) {
    let m = p.marker();
    if !p.eat_if(T![:]) {
        p.wrap(m, SyntaxKind::ParentClassList);
        return;
    }

    while !p.eof() && p.at(TokenKind::Id) {
        class_ref(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::ParentClassList);
}

// ClassRef ::= Identifier ( "<" ArgValueList? ">" )?
fn class_ref(p: &mut Parser) {
    let m = p.marker();
    identifier(p);
    if p.eat_if(T![<]) {
        arg_value_list(p);
        p.expect(T![>]);
    }
    p.wrap(m, SyntaxKind::ClassRef);
}

// ArgValueList ::= PositionalArgValueList ","? NamedArgValueList
fn arg_value_list(p: &mut Parser) {
    let m = p.marker();
    positional_arg_value_list(p);
    p.wrap(m, SyntaxKind::ArgValueList);
}

// PositionalArgValueList ::= ( Value ( "," Value )* ) ?
fn positional_arg_value_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() && !p.at(T![>]) {
        value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::PositionalArgValueList);
}

fn body(p: &mut Parser) {
    let m = p.marker();
    if p.eat_if(T![;]) {
        p.wrap(m, SyntaxKind::Body);
        return;
    }

    p.expect(T!['{']);
    while !p.eof() && !p.at(T!['}']) {
        let prev_cursor = p.current_start();
        body_item(p);
        if p.current_start() == prev_cursor {
            p.error_and_eat("unexpected token");
        }
    }
    p.expect(T!['}']);
    p.wrap(m, SyntaxKind::Body);
}

// Body ::= ";" | "{" BodyItem* "}"
fn body_item(p: &mut Parser) {
    match p.current() {
        T![let] => field_let(p),
        _ => field_def(p),
    }
}

// FieldDef ::= ( Type | CodeType ) Identifier ( "=" Value )? ";"
fn field_def(p: &mut Parser) {
    let m = p.marker();
    if p.at(T![code]) {
        code_type(p);
    } else {
        r#type(p);
    }
    identifier(p);
    if p.eat_if(T![=]) {
        value(p);
    }
    p.expect(T![;]);
    p.wrap(m, SyntaxKind::FieldDef);
}

// CodeType ::= "code"
fn code_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![code]);
    p.wrap(m, SyntaxKind::CodeType);
}

// FieldLet ::= "let" Identitfer ( "{" RangeList "}" )? "=" Value ";"
fn field_let(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![let]);
    identifier(p);
    p.expect(T![=]);
    value(p);
    p.expect(T![;]);
    p.wrap(m, SyntaxKind::FieldLet);
}

// Type ::= BitType | IntType | StringType | DagType | BitsType | ListType | ClassId
fn r#type(p: &mut Parser) {
    match p.current() {
        T![bit] => bit_type(p),
        T![int] => int_type(p),
        T![string] => string_type(p),
        T![dag] => dag_type(p),
        T![bits] => bits_type(p),
        T![list] => list_type(p),
        TokenKind::Id => class_id(p),
        _ => p.error_and_eat("expected type"),
    }
}

// BitType ::= "bit"
fn bit_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![bit]);
    p.wrap(m, SyntaxKind::BitType);
}

// IntType ::= "int"
fn int_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![int]);
    p.wrap(m, SyntaxKind::IntType);
}

// StringType ::= "string"
fn string_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![string]);
    p.wrap(m, SyntaxKind::StringType);
}

// DagType ::= "dag"
fn dag_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![dag]);
    p.wrap(m, SyntaxKind::DagType);
}

// BitsType ::= "bits" "<" Integer ">"
fn bits_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![bits]);
    p.expect(T![<]);
    integer(p);
    p.expect(T![>]);
    p.wrap(m, SyntaxKind::BitsType);
}

// ListType ::= "list" "<" Type ">"
fn list_type(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![list]);
    p.expect(T![<]);
    r#type(p);
    p.expect(T![>]);
    p.wrap(m, SyntaxKind::ListType);
}

// ClassId ::= Identifier
fn class_id(p: &mut Parser) {
    let m = p.marker();
    identifier(p);
    p.wrap(m, SyntaxKind::ClassId);
}

// Value ::= SimpleValue ValueSuffix* | Value "#" Value?
fn value(p: &mut Parser) {
    let m = p.marker();
    simple_value(p);

    loop {
        // ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
        match p.current() {
            T![.] => field_suffix(p),
            _ => break,
        }
    }

    p.wrap(m, SyntaxKind::Value);
}

// FieldSuffix ::= "." Identifier
fn field_suffix(p: &mut Parser) {
    let m = p.marker();
    p.assert(T![.]);
    identifier(p);
    p.wrap(m, SyntaxKind::FieldSuffix);
}

// SimpleValue ::= Integer | String | Code | Boolean | Uninitialized | Bits | List | Dag | Identifier | ClassValue | BangOperator | CondOperator
fn simple_value(p: &mut Parser) {
    match p.current() {
        TokenKind::IntVal => integer(p),
        TokenKind::StrVal => string(p),
        TokenKind::CodeFragment => code(p),
        T![true] | T![false] => boolean(p),
        T![?] => uninitialized(p),
        T!['{'] => bits(p),
        T!['['] => list(p),
        T!['('] => dag(p),
        TokenKind::Id => identifier(p),
        kind if kind.is_bang_operator() => bang_operator(p),
        kind if kind.is_cond_operator() => cond_operator(p),
        _ => p.error_and_eat("unexpected"),
    }
}

// Integer ::= INT
fn integer(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::IntVal);
    p.wrap(m, SyntaxKind::Integer);
}

// String ::= STRING
fn string(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::StrVal);
    p.wrap(m, SyntaxKind::String);
}

// Code ::= CODE
fn code(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::CodeFragment);
    p.wrap(m, SyntaxKind::Code);
}

// Boolean ::= "true" | "false"
fn boolean(p: &mut Parser) {
    let m = p.marker();
    if !(p.eat_if(T![true]) || p.eat_if(T![false])) {
        p.error_and_eat("expected true or false");
    }
    p.wrap(m, SyntaxKind::Boolean);
}

// Uninitialized ::= "?"
fn uninitialized(p: &mut Parser) {
    let m = p.marker();
    p.expect(T![?]);
    p.wrap(m, SyntaxKind::Uninitialized);
}

// Bits ::= "{" ValueList "}"
fn bits(p: &mut Parser) {
    let m = p.marker();
    p.expect(T!['{']);
    value_list(p, T!['}']);
    p.wrap(m, SyntaxKind::Bits);
}

// ValueList ::= Value ( "," Value )*
fn value_list(p: &mut Parser, terminator: TokenKind) {
    let m = p.marker();
    while !p.eof() && !p.at(terminator) {
        value(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::ValueList);
    p.expect(terminator);
}

// List ::= "[" ValueList "]"
fn list(p: &mut Parser) {
    let m = p.marker();
    p.expect(T!['[']);
    value_list(p, T![']']);
    p.wrap(m, SyntaxKind::List);
}

// Dag ::= ( DagArg DagArgList? )
fn dag(p: &mut Parser) {
    let m = p.marker();
    p.expect(T!['(']);
    dagarg(p);
    if !p.at(T![')']) {
        dagarg_list(p);
    }
    p.expect(T![')']);
    p.wrap(m, SyntaxKind::Dag);
}

// DagArgList ::= DagArg ( "," DagArg )*
fn dagarg_list(p: &mut Parser) {
    let m = p.marker();
    while !p.eof() {
        dagarg(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::DagArgList);
}

// DagArg ::= Value ( ":" VARNAME ) | VARNAME
fn dagarg(p: &mut Parser) {
    let m = p.marker();
    if p.eat_if(TokenKind::VarName) {
        p.wrap(m, SyntaxKind::DagArg);
        return;
    }

    value(p);
    if p.eat_if(T![:]) {
        var_name(p);
    }
    p.wrap(m, SyntaxKind::DagArg);
}

// VarName ::= VARNAME
fn var_name(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::VarName);
    p.wrap(m, SyntaxKind::VarName);
}

// Identifier ::= ID
fn identifier(p: &mut Parser) {
    let m = p.marker();
    p.expect(TokenKind::Id);
    p.wrap(m, SyntaxKind::Identifier);
}

// BangOperator ::= BANGOP "(" ValueList ")"
fn bang_operator(p: &mut Parser) {
    let m = p.marker();
    if !p.current().is_bang_operator() {
        p.error_and_eat("expected bang operator");
    } else {
        p.eat();
        p.expect(T!['(']);
        while !p.eof() && !p.at(T![')']) {
            value(p);
            if !p.eat_if(T![,]) {
                break;
            }
        }
        p.expect(T![')']);
    }
    p.wrap(m, SyntaxKind::BangOperator);
}

// CondOperator ::= CONDOP "(" CondClause ( "," CondClause )* ")"
fn cond_operator(p: &mut Parser) {
    let m = p.marker();
    p.expect(T![!cond]);
    p.expect(T!['(']);
    while !p.eof() && !p.at(T![')']) {
        cond_clause(p);
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.expect(T![')']);
    p.wrap(m, SyntaxKind::CondOperator);
}

// CondClause ::= Value ":" Value
fn cond_clause(p: &mut Parser) {
    let m = p.marker();
    if !p.eat_if(TokenKind::VarName) {
        value(p);
        p.expect(T![:]);
        value(p);
    }

    p.wrap(m, SyntaxKind::CondClause);
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn include() {
        insta::assert_display_snapshot!(parse(r#"include "foo.td""#))
    }

    #[test]
    fn class() {
        insta::assert_display_snapshot!(parse("class Foo<int A, int B = 1>: Bar<A, 2>;"));
    }

    #[test]
    fn class_with_body() {
        insta::assert_display_snapshot!(parse(
            "class Foo<int A> {
                int B;
                int C = A;
                let D = A;
            }"
        ))
    }

    #[test]
    fn def() {
        insta::assert_display_snapshot!(parse("def Foo : Bar;"))
    }

    #[test]
    fn r#let() {
        insta::assert_display_snapshot!(parse("let A = 1 in { class Foo; }"))
    }

    #[test]
    fn r#type() {
        insta::assert_display_snapshot!(parse(
            "class Foo<bit A, int B, string C, dag D, bits<32> E, list<int> F, Bar G>;"
        ))
    }

    #[test]
    fn value() {
        insta::assert_display_snapshot!(parse("class Foo<int A = Hoge.Fuga>;"));
    }

    #[test]
    fn simple_value() {
        insta::assert_display_snapshot!(parse(
            "class Foo<int A = 1, string B = \"hoge\", bit D = false, int E = ?, bits<2> F = {0, 1}, list<int> G = [1, 2], dag H = (add A:$hoge), int I = A, int J = !add(A, B), int K = !cond(false: 1, true: 2)> {
                code C = [{ true }];
            }"
        ))
    }
}
