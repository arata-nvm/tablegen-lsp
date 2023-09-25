File ::= StatementList
StatementList ::= Statement*
Statement ::= Include | Assert | Class | Def | Defm | Defset | Defvar | Foreach | If | Let | MultiClass

Include ::= "include" String

Class ::= "class" Identifier TemplateArgList? RecordBody

Def ::= "def" Value? RecordBody

Let ::= "let" LetList "in" ( "{" Statement* "}" | Statement )
LetList ::= LetItem ( "," LetItem )*
LetItem ::= Identifier ( "<" RangeList ">" )? "=" Value

MultiClass ::= "multiclass" Identifier TemplateArgList? ParentClassList "{" MultiClassStatement+ "}"
MultiClassStatement ::= Assert | Def | Defm | Defvar | Foreach | If | Let

Defm ::= "defm" Value? ParentClassList ";"

Defset ::= "defset" Type Identifier "=" "{" Statement* "}"

Defvar ::= "defvar" Identifier "=" Value ";"

Foreach ::= "foreach" ForeachIterator "in" ( "{" Statement* "}" | Statement )
ForeachIterator ::= Identifier "=" ( "{" RangeList "}" | RangePiece | Value )

If ::= "if" Value "then" ( "{" Statement* "}" | Statement )

Assert ::= "assert" Value "," Value ";"

TemplateArgList ::= "<" TemplateArgDecl ( "," TemplateArgDecl )* ">"
TemplateArgDecl ::= Type Identifier ( "=" Value )?

RecordBody ::= ParentClassList Body
ParentClassList ::= ( ":" ClassRef ( "," ClassRef )* )?
ClassRef ::= Identifier ( "<" ArgValueList? ">" )?
ArgValueList ::= PositionalArgValueList ","? NamedArgValueList
PositionalArgValueList ::= ( Value ( "," Value )* ) ?
NamedArgValueList ::= ( Value "=" Value ( "," Value "=" Value )* )?

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

Value ::= SimpleValue ValueSuffix* | Value "#" Value?
ValueSuffix ::= RangeSuffix | SliceSuffix | FieldSuffix
RangeSuffix ::= "{" RangeList "}"
RangeList ::= RangePiece ( "," RangePiece )*
RangePiece ::= Integer | Integer "..." Integer | Integer "-" Integer | Integer Integer
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
List ::= "[" ValueList "]"
Dag ::= ( DagArg DagArgList? )
DagArgList ::= DagArg ( "," DagArg )*
DagArg ::= Value ( ":" VARNAME ) | VARNAME
VarName ::= VARNAME
Identifier ::= ID
ClassValue ::= ClassID "<" ArgValueList ">"
BangOperator ::= BANGOP "(" ValueList ")"
CondOperator ::= CONDOP "(" CondClause ( "," CondClause )* ")"
CondClause ::= Value ":" Value