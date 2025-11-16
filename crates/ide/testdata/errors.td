// === Undefined Symbol Errors ===

// Undefined class reference
 def undefined_class_ref : UndefinedClass;

// Undefined variable reference
defvar uses_undefined = undefined_variable;

// Undefined field access
class HasField {
  int existing_field;
}

// Nonexistent field access
defvar bad_field_access = HasField<>.nonexistent_field;

// Nonexistent field let
def bad_field_let : HasField {
  let nonexistent_field = 10;
}

// === Error Recovering ===

class Foo<int a = nonexistent_var> {
  int b = nonexistent_var;
}

class Bar: Foo<nonexistent_var>;

// === Type Mismatch Errors ===

class TypeMismatch {
  int int_field = "not_an_int";
  string string_field = 1;
  list<int> int_list = ["string", "values"];
}

// === Duplicate Definition Errors ===

class duplicate_class;
class duplicate_class;  // Duplicate class

def duplicate_def;
def duplicate_def;  // Duplicate def

class duplicate_template_arg<int x, int x>; // Duplicate template argument

class duplicate_field {
  int x;
  int x; // Duplicate field
}

defvar duplicate_var = 10;
defvar duplicate_var = 10; // Duplicate variable
defvar duplicate_def = 10; // Duplicate variable

class for_dup_test;
defset list<for_dup_test> duplicate_defset = {};
defset list<for_dup_test> duplicate_defset = {}; // Duplicate defset

multiclass duplicate_multiclass;
multiclass duplicate_multiclass; // Duplicate multiclass

// === Cyclic Inheritance Errors ===

// Direct cycle
class CyclicA : CyclicA;

// Indirect cycle
class CyclicC;
class CyclicB : CyclicC;
class CyclicC : CyclicB;

// === Template Argument Count Mismatch ===

class RequiresArgs<int x, string y> {
  int value = x;
  string name = y;
}

// Missing arguments
def missing_args : RequiresArgs;

// Partial arguments
def partial_args : RequiresArgs<10>;

// Too many arguments
def too_many_args : RequiresArgs<1, 2, 3>;

// === Field Type Incompatibility ===

class Base;
class Derived : Base;
class Unrelated;

class FieldIncompat {
  // Cannot assign base to derived
  Derived derived_field1 = Base<>;
  Derived derived_field2 = Unrelated<>;
}

// === Named Argument Errors ===

class NamedArgs<int x, int y>;

// Duplicate named argument
def dup_named : NamedArgs<x=1, x=2>;

// Unknown named argument
def unknown_named : NamedArgs<z=1>;
