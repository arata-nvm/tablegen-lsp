class BaseClass {
  int value = 0;
}

class TemplatedClass<int x> {
  int param = x;
}

// Simple named def
def simple_def;

// Def with parent class
def def_with_parent : BaseClass;

// Def with field definition
def def_with_fields : BaseClass {
  string name = "test";
  bit flag = 1;
}

// Def with field override
def def_with_override : BaseClass {
  let value = 42;
}

// Def with multiple parents
class Mixin1;
class Mixin2;
def multi_parent : Mixin1, Mixin2;

// Def with template argument
def templated_def : TemplatedClass<100>;

// Anonymous def
def : BaseClass;

// Def in foreach
foreach i = 1...3 in {
  def loop_def#i : BaseClass {
    let value = i;
  }
}

// Def in defset
defset list<BaseClass> my_set = {
  def set_member1 : BaseClass;
  def set_member2 : BaseClass;
}

// Def with complex name expression
def PREFIX_#"MIDDLE"#_SUFFIX : BaseClass;
