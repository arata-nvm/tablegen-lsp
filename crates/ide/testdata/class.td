// Simple class
class SimpleClass;

// Class with fields
class ClassWithFields {
  int field1;
  string field2;
  bit field3;
}

// Class with template arguments
class TemplatedClass<int size, string name> {
  int Size = size;
  string Name = name;
}

// Class with default template arguments
class DefaultTemplateArgs<int x = 5, string y = "default">;

// Class inheritance
class Base {
  int base_field = 1;
}

class Derived : Base {
  let base_field = 2;
  int derived_field = 2;
}

// Multiple inheritance
class Mixin1 {
  int mixin1_field;
}

class Mixin2 {
  int mixin2_field;
}

class MultiInherit : Mixin1, Mixin2 {
  int own_field;
}

// Parameterized inheritance
class ParamBase<int x>;
class ParamDerived : ParamBase<10>;

// Nested field types
class Inner {
  int inner_value;
}

class Outer {
  Inner inner_field;
}

// Class with list type
class ListClass {
  list<int> numbers;
  list<SimpleClass> objects;
}

// Class with defvar
class DefVarClass {
  defvar defvar_field = 1;
  int regular_field = defvar_field;
}

// Class with assert
class AssertClass {
  int value;
  assert value, "Value must be positive";
}
