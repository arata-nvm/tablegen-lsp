use crate::{
    kind::{SyntaxKind, TokenKind},
    node::SyntaxNode,
    parser::{Parser, Result, ResultExt},
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
    if statement_list(p).is_err() || !p.eof() {
        p.error("unexpected input at top level");
    }
    p.wrap_all(m, SyntaxKind::File);
}

// StatementList ::= Statement*
fn statement_list(p: &mut Parser) -> Result {
    let m = p.marker();
    while !p.eof() {
        statement(p)?;
    }
    p.wrap(m, SyntaxKind::StatementList);
    Ok(())
}

// Statement ::= Include | Assert | Class | Def | Defm | Defset | Defvar | Foreach | If | Let | MultiClass
fn statement(p: &mut Parser) -> Result {
    match p.current() {
        T![include] => include(p),
        T![class] => class(p),
        T![def] => def(p),
        T![let] => r#let(p),
        _ => {
            p.error_and_eat("expected class, def, defm, defset, multiclass, let or foreach");
            Err(())
        }
    }
}

// Include ::= "include" String
fn include(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![include]);
    string(p).or_error(p, "expected filename after include")?;
    p.wrap(m, SyntaxKind::Include);
    Ok(())
}

// Class ::= "class" Identifier TemplateArgList? RecordBody
fn class(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![class]);
    identifier(p).or_error(p, "expected class name after 'class' keyword")?;
    opt_template_arg_list(p)?;
    record_body(p)?;
    p.wrap(m, SyntaxKind::Class);
    Ok(())
}

// Def ::= "def" Value? RecordBody
fn def(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![def]);
    value(p)?;
    record_body(p)?;
    p.wrap(m, SyntaxKind::Def);
    Ok(())
}

// Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement )
fn r#let(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![let]);
    let_list(p)?;
    if !p.eat_if(T![in]) {
        p.error("expected 'in' at end of top-level 'let'");
        p.wrap_all(m, SyntaxKind::Let);
        return Err(());
    }
    if p.eat_if(T!['{']) {
        while !p.eof() && !p.at(T!['}']) {
            statement(p)?;
        }
        p.expect_with_msg(T!['}'], "expected '}' at end of top-level let command")?;
    } else {
        statement(p)?;
    }
    p.wrap_all(m, SyntaxKind::Let);
    Ok(())
}

// LetList ::= LetItem ( "," LetItem )*
fn let_list(p: &mut Parser) -> Result {
    let m = p.marker();
    while !p.eof() {
        let_item(p)?;
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::LetList);
    Ok(())
}

// LetItem ::= Identifier ( "<" RangeList ">" )? "=" Value
fn let_item(p: &mut Parser) -> Result {
    let m = p.marker();
    identifier(p).or_error(p, "expected identifier in let expression")?;
    if !p.eat_if(T![=]) {
        p.error("expected '=' in let expression");
        return Err(());
    }
    value(p)?;
    p.wrap(m, SyntaxKind::LetItem);
    Ok(())
}

fn opt_template_arg_list(p: &mut Parser) -> Result {
    if p.at(T![<]) {
        template_arg_list(p)?;
    }
    Ok(())
}

// TemplateArgList ::= "<" TemplateArgDecl ( "," TemplateArgDecl )* ">"
fn template_arg_list(p: &mut Parser) -> Result {
    let m = p.marker();
    delimited(p, T![<], T![>], T![,], template_arg_decl)?;
    p.wrap(m, SyntaxKind::TemplateArgList);
    Ok(())
}

// TemplateArgDecl ::= Type Identifier ( "=" Value )?
fn template_arg_decl(p: &mut Parser) -> Result {
    let m = p.marker();
    r#type(p)?;
    identifier(p).or_error(p, "expected identifier in declaration")?;
    if p.eat_if(T![=]) {
        value(p)?;
    }
    p.wrap(m, SyntaxKind::TemplateArgDecl);
    Ok(())
}

// RecordBody ::= ParentClassList Body
fn record_body(p: &mut Parser) -> Result {
    let m = p.marker();
    parent_class_list(p)?;
    body(p)?;
    p.wrap(m, SyntaxKind::RecordBody);
    Ok(())
}

// ParentClassList ::= ( ":" ClassRef ( "," ClassRef )* )?
fn parent_class_list(p: &mut Parser) -> Result {
    let m = p.marker();
    if !p.eat_if(T![:]) {
        p.wrap(m, SyntaxKind::ParentClassList);
        return Ok(());
    }

    while !p.eof() && p.at(TokenKind::Id) {
        class_ref(p)?;
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::ParentClassList);
    Ok(())
}

// ClassRef ::= Identifier ( "<" ArgValueList? ">" )?
fn class_ref(p: &mut Parser) -> Result {
    let m = p.marker();
    identifier(p)?;
    if p.eat_if(T![<]) {
        arg_value_list(p)?;
        p.expect_with_msg(T![>], "expected '>' in template value list")?;
    }
    p.wrap(m, SyntaxKind::ClassRef);
    Ok(())
}

// ArgValueList ::= PositionalArgValueList ","? NamedArgValueList
fn arg_value_list(p: &mut Parser) -> Result {
    let m = p.marker();
    positional_arg_value_list(p)?;
    p.wrap(m, SyntaxKind::ArgValueList);
    Ok(())
}

// PositionalArgValueList ::= ( Value ( "," Value )* ) ?
fn positional_arg_value_list(p: &mut Parser) -> Result {
    let m = p.marker();
    while !p.eof() && !p.at(T![>]) {
        value(p)?;
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::PositionalArgValueList);
    Ok(())
}

// Body ::= ";" | "{" BodyItem* "}"
fn body(p: &mut Parser) -> Result {
    let m = p.marker();
    if p.eat_if(T![;]) {
        p.wrap(m, SyntaxKind::Body);
        return Ok(());
    }

    p.expect_with_msg(T!['{'], "expected ';' or '{' to start body")?;
    while !p.eof() && !p.at(T!['}']) {
        body_item(p)?;
    }
    p.expect(T!['}'])?;
    p.wrap(m, SyntaxKind::Body);
    Ok(())
}

// BodyItem ::= FieldDef | FieldLet | Defvar | Assert
fn body_item(p: &mut Parser) -> Result {
    match p.current() {
        T![let] => field_let(p),
        _ => field_def(p),
    }
}

// FieldDef ::= ( Type | CodeType ) Identifier ( "=" Value )? ";"
fn field_def(p: &mut Parser) -> Result {
    let m = p.marker();
    if p.at(T![code]) {
        code_type(p)?;
    } else {
        r#type(p)?;
    }
    identifier(p).or_error(p, "expected identifier in declaration")?;
    if p.eat_if(T![=]) {
        value(p)?;
    }
    p.expect_with_msg(T![;], "expected ';' after declaration")?;
    p.wrap(m, SyntaxKind::FieldDef);
    Ok(())
}

// CodeType ::= "code"
fn code_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![code]);
    p.wrap(m, SyntaxKind::CodeType);
    Ok(())
}

// FieldLet ::= "let" Identitfer ( "{" RangeList "}" )? "=" Value ";"
fn field_let(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![let]);
    identifier(p).or_error(p, "expected field identifier after let")?;
    p.expect(T![=])?;
    value(p).or_error(p, "expected '=' in let expression")?;
    p.expect_with_msg(T![;], "expected ';' after let expression")?;
    p.wrap(m, SyntaxKind::FieldLet);
    Ok(())
}

// Type ::= BitType | IntType | StringType | DagType | BitsType | ListType | ClassId
fn r#type(p: &mut Parser) -> Result {
    match p.current() {
        T![bit] => bit_type(p),
        T![int] => int_type(p),
        T![string] => string_type(p),
        T![dag] => dag_type(p),
        T![bits] => bits_type(p),
        T![list] => list_type(p),
        TokenKind::Id => class_id(p),
        _ => {
            p.error_and_eat("unknown token when expecting a type");
            Err(())
        }
    }
}

// BitType ::= "bit"
fn bit_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![bit]);
    p.wrap(m, SyntaxKind::BitType);
    Ok(())
}

// IntType ::= "int"
fn int_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![int]);
    p.wrap(m, SyntaxKind::IntType);
    Ok(())
}

// StringType ::= "string"
fn string_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![string]);
    p.wrap(m, SyntaxKind::StringType);
    Ok(())
}

// DagType ::= "dag"
fn dag_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![dag]);
    p.wrap(m, SyntaxKind::DagType);
    Ok(())
}

// BitsType ::= "bits" "<" Integer ">"
fn bits_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![bits]);
    p.expect_with_msg(T![<], "expected '<' after bits type")?;
    integer(p).or_error(p, "expected integer in bits<n> type")?;
    p.expect_with_msg(T![>], "expected '>' at end of bits<n> type")?;
    p.wrap(m, SyntaxKind::BitsType);
    Ok(())
}

// ListType ::= "list" "<" Type ">"
fn list_type(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![list]);
    p.expect_with_msg(T![<], "expected '<' after list type")?;
    r#type(p)?;
    p.expect_with_msg(T![>], "expected '>' at end of list<ty> type")?;
    p.wrap(m, SyntaxKind::ListType);
    Ok(())
}

// ClassId ::= Identifier
fn class_id(p: &mut Parser) -> Result {
    let m = p.marker();
    identifier(p).or_error(p, "expected name for ClassID")?;
    p.wrap(m, SyntaxKind::ClassId);
    Ok(())
}

// Value ::= SimpleValue ValueSuffix* | Value "#" Value?
fn value(p: &mut Parser) -> Result {
    let m = p.marker();
    simple_value(p)?;

    loop {
        // ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
        match p.current() {
            T![.] => field_suffix(p)?,
            _ => break,
        }
    }

    p.wrap(m, SyntaxKind::Value);
    Ok(())
}

// FieldSuffix ::= "." Identifier
fn field_suffix(p: &mut Parser) -> Result {
    let m = p.marker();
    p.assert(T![.]);
    identifier(p).or_error(p, "expected field identifier after '.'")?;
    p.wrap(m, SyntaxKind::FieldSuffix);
    Ok(())
}

// SimpleValue ::= Integer | String | Code | Boolean | Uninitialized | Bits | List | Dag | Identifier | ClassValue | BangOperator | CondOperator
fn simple_value(p: &mut Parser) -> Result {
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
        _ => {
            p.error_and_eat("unknown token when parsing a value");
            Err(())
        }
    }
}

// Integer ::= INT
fn integer(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(TokenKind::IntVal)?;
    p.wrap(m, SyntaxKind::Integer);
    Ok(())
}

// String ::= STRING
fn string(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(TokenKind::StrVal)?;
    p.wrap(m, SyntaxKind::String);
    Ok(())
}

// Code ::= CODE
fn code(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(TokenKind::CodeFragment)?;
    p.wrap(m, SyntaxKind::Code);
    Ok(())
}

// Boolean ::= "true" | "false"
fn boolean(p: &mut Parser) -> Result {
    let m = p.marker();
    if !(p.eat_if(T![true]) || p.eat_if(T![false])) {
        p.error("expected true or false");
        return Err(());
    }
    p.wrap(m, SyntaxKind::Boolean);
    Ok(())
}

// Uninitialized ::= "?"
fn uninitialized(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(T![?])?;
    p.wrap(m, SyntaxKind::Uninitialized);
    Ok(())
}

// Bits ::= "{" ValueList "}"
fn bits(p: &mut Parser) -> Result {
    let m = p.marker();
    value_list(p, T!['{'], T!['}'])?;
    p.wrap(m, SyntaxKind::Bits);
    Ok(())
}

// List ::= "[" ValueList "]"
fn list(p: &mut Parser) -> Result {
    let m = p.marker();
    value_list(p, T!['['], T![']'])?;
    p.wrap(m, SyntaxKind::List);
    Ok(())
}

// ValueList ::= Value ( "," Value )*
fn value_list(p: &mut Parser, bra: TokenKind, ket: TokenKind) -> Result {
    let m = p.marker();
    delimited(p, bra, ket, T![,], value)?;
    p.wrap(m, SyntaxKind::ValueList);
    Ok(())
}

// Dag ::= ( DagArg DagArgList? )
fn dag(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(T!['('])?;
    if !p.at(TokenKind::Id) {
        p.error("expected identifier in dag init");
        return Err(());
    }
    dagarg(p)?;
    if !p.at(T![')']) {
        dagarg_list(p)?;
    }
    p.expect_with_msg(T![')'], "expected ')' in dag init")?;
    p.wrap(m, SyntaxKind::Dag);
    Ok(())
}

// DagArgList ::= DagArg ( "," DagArg )*
fn dagarg_list(p: &mut Parser) -> Result {
    let m = p.marker();
    while !p.eof() {
        dagarg(p)?;
        if !p.eat_if(T![,]) {
            break;
        }
    }
    p.wrap(m, SyntaxKind::DagArgList);
    Ok(())
}

// DagArg ::= Value ( ":" VARNAME ) | VARNAME
fn dagarg(p: &mut Parser) -> Result {
    let m = p.marker();
    if p.eat_if(TokenKind::VarName) {
        p.wrap(m, SyntaxKind::DagArg);
        return Ok(());
    }

    value(p)?;
    if p.eat_if(T![:]) {
        var_name(p).or_error(p, "expected variable name in dag literal")?;
    }
    p.wrap(m, SyntaxKind::DagArg);
    Ok(())
}

// VarName ::= VARNAME
fn var_name(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(TokenKind::VarName)?;
    p.wrap(m, SyntaxKind::VarName);
    Ok(())
}

// Identifier ::= ID
fn identifier(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(TokenKind::Id)?;
    p.wrap(m, SyntaxKind::Identifier);
    Ok(())
}

// BangOperator ::= BANGOP "(" ValueList ")"
fn bang_operator(p: &mut Parser) -> Result {
    if !p.current().is_bang_operator() {
        p.error_and_eat("expected bang operator");
        return Err(());
    }

    let m = p.marker();
    p.eat(); // eat bang operator
    delimited(p, T!['('], T![')'], T![,], value)?;
    p.wrap(m, SyntaxKind::BangOperator);
    Ok(())
}

// CondOperator ::= CONDOP "(" CondClause ( "," CondClause )* ")"
fn cond_operator(p: &mut Parser) -> Result {
    let m = p.marker();
    p.expect(T![!cond])?;
    delimited(p, T!['('], T![')'], T![,], |p| cond_clause(p))?;
    p.wrap(m, SyntaxKind::CondOperator);
    Ok(())
}

// CondClause ::= Value ":" Value
fn cond_clause(p: &mut Parser) -> Result {
    let m = p.marker();
    value(p)?;
    p.expect(T![:])?;
    value(p)?;
    p.wrap(m, SyntaxKind::CondClause);
    Ok(())
}

fn delimited<F>(
    p: &mut Parser,
    bra: TokenKind,
    ket: TokenKind,
    delim: TokenKind,
    mut parser: F,
) -> Result
where
    F: FnMut(&mut Parser<'_>) -> Result,
{
    p.expect(bra)?;
    while !p.at(ket) && !p.eof() {
        parser(p)?;

        if !p.eat_if(delim) {
            break;
        }
    }
    p.expect(ket)?;
    Ok(())
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
