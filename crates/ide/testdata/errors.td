// === Undefined Symbol Errors ===

// Undefined class reference
 def undefined_class_ref : UndefinedClass;

// Undefined variable reference
// FIXME: 定義されない
defvar uses_undefined = undefined_variable;

// Undefined field access
class HasField {
  int existing_field;
}

// FIXME: 定義されない
defvar bad_field_access = HasField<>.nonexistent_field;

def bad_field_let : HasField {
  let nonexistent_field = 10;
}

// === Type Mismatch Errors ===

class TypeMismatch {
  int int_field = "not_an_int";
  string string_field = 1;
  list<int> int_list = ["string", "values"];
}

// === Duplicate Definition Errors ===

class DuplicateTest;
class DuplicateTest;  // Duplicate class

def duplicate_def;
// FIXME: 定義されてしまう
def duplicate_def;  // Duplicate def

// === Cyclic Inheritance Errors ===

// Direct cycle
class CyclicA : CyclicA;

// Indirect cycle
// FIXME: エラーが出ない
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

// === Invalid Bits Size ===

class InvalidBits {
  // FIXME: エラーが出ない
  bits<"not_a_number"> bad_bits;
}

// === Named Argument Errors ===

class NamedArgs<int x, int y>;

// Duplicate named argument
// FIXME: エラーが正しくない
def dup_named : NamedArgs<x=1, x=2>;

// Unknown named argument
// FIXME: エラーが正しくない
def unknown_named : NamedArgs<z=1>;
