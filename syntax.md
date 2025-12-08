SourceFile ::= StatementList
StatementList ::= Statement*
Statement ::= Include | Assert | Class | Def | Defm | Defset | Deftype | Defvar | Dump | Foreach | If | Let | MultiClass

Include ::= "include" String

Class ::= "class" Identifier TemplateArgList? RecordBody

Def ::= "def" Value? RecordBody

Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement )
LetList ::= LetItem ( "," LetItem )*
LetItem ::= Identifier ( "<" RangeList ">" )? "=" Value

MultiClass ::= "multiclass" Identifier TemplateArgList? ParentClassList ( ";" | "{" MultiClassStatement+ "}" )
MultiClassStatement ::= Assert | Def | Defm | Defvar | Dump | Foreach | Let | If

Defm ::= "defm" Value? ParentClassList ";"

Defset ::= "defset" Type Identifier "=" "{" Statement* "}"

Deftype ::= "deftype" Identifier "=" Type ";"

Defvar ::= "defvar" Identifier "=" Value ";"

Dump ::= "dump" Value ";"

Foreach ::= "foreach" ForeachIterator "in" ( "{" Statement* "}" | Statement )
ForeachIterator ::= Identifier "=" ForeachIteratorInit
ForeachIteratorInit ::= "{" RangeList "}" | RangePiece | Value

If ::= "if" Value "then" ( "{" Statement* "}" | Statement ) ( "else" ( "{" Statement* "}" | Statement ) )?

Assert ::= "assert" Value "," Value ";"

TemplateArgList ::= "<" TemplateArgDecl ( "," TemplateArgDecl )* ">"
TemplateArgDecl ::= Type Identifier ( "=" Value )?

RecordBody ::= ParentClassList Body
ParentClassList ::= ( ":" ClassRef ( "," ClassRef )* )?
ClassRef ::= Identifier ( "<" ArgValueList? ">" )?
ArgValueList ::= ( ArgValue ( "," ArgValue )* )?
ArgValue ::= PositionalArgValue | NamedArgValue
PositionalArgValue ::= Value
NamedArgValue ::= Value "=" Value

Body ::= ";" | "{" BodyItem* "}"
BodyItem ::= FieldDef | FieldLet | Defvar | Assert
FieldDef ::= ( Type | CodeType ) Identifier ( "=" Value )? ";"
CodeType ::= "code"
FieldLet ::= "let" Identifier ( "{" RangeList "}" )? "=" Value ";"

Type ::= BitType | IntType | StringType | DagType | BitsType | ListType | ClassId
BitType ::= "bit"
IntType ::= "int"
StringType ::= "string"
DagType ::= "dag"
BitsType ::= "bits" "<" Integer ">"
ListType ::= "list" "<" Type ">"
ClassId ::= Identifier

Value ::= InnerValue ( "#" InnerValue )*
InnerValue ::= SimpleValue ValueSuffix*
ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
RangeSuffix ::= "{" RangeList "}"
RangeList ::= RangePiece ( "," RangePiece )*
RangePiece ::= Value | Value "..." Value | Value "-" Value | Value Value
SliceSuffix ::= "[" SliceElements "]"
SliceElements ::= ( SliceElement "," )* SliceElement ","?
SliceElement ::= Value | Value "..." Value | Value "-" Value | Value Integer
FieldSuffix ::= "." Identifier

SimpleValue ::= Integer | String | Code | Boolean | Uninitialized | Bits | List | Dag | Identifier | ClassValue | BangOperator | CondOperator
Integer ::= INT
String ::= STRING
Code ::= CODE
Boolean ::= "true" | "false"
Uninitialized ::= "?"
Bits ::= "{" ValueList "}"
ValueList ::= Value ( "," Value )*
List ::= "[" ValueList "]" ( "<" Type ">" )?
Dag ::= ( DagArg DagArgList? )
DagArgList ::= DagArg ( "," DagArg )*
DagArg ::= Value ( ":" VARNAME ) | VARNAME
VarName ::= VARNAME
Identifier ::= ID
ClassValue ::= ClassID "<" ArgValueList ">"
BangOperator ::= BANGOP "(" ValueList ")"
CondOperator ::= CONDOP "(" CondClause ( "," CondClause )* ")"
CondClause ::= Value ":" Value
