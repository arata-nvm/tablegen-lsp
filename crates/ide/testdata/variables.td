class BaseClass {
  int value;
}

// Simple defvar
defvar simple_var = 42;
defvar string_var = "hello";
defvar list_var = [1, 2, 3];

// Defvar with record reference
def my_def : BaseClass {
  let value = 10;
}

defvar record_var = my_def;

// Foreach with range
foreach i = 0...4 in {
  def range_def#i : BaseClass {
    let value = i;
  }
}

// Foreach with list
foreach item = [10, 20, 30] in {
  defvar local_var = item;
  def list_def#item : BaseClass {
    let value = item;
  }
}

// Foreach with range piece
foreach x = 1...3 in {
  foreach y = 1...3 in {
    def nested_def#x#_#y : BaseClass {
      let value = !add(x, y);
    }
  }
}

// Nested defvar scopes
defvar outer_var = 100;
foreach i = 1...2 in {
  defvar inner_var = !add(outer_var, i);
  def scope_test#i : BaseClass {
    let value = inner_var;
  }
}

// Defvar in class template argument
class TemplateClass<int x> {
  int param = x;
}

defvar computed_value = !add(10, 20);
def template_with_var : TemplateClass<computed_value>;

// Defvar with bang operator
defvar concat_result = !strconcat("prefix_", "suffix");
defvar add_result = !add(5, 10);
defvar list_concat = !listconcat([1, 2], [3, 4]);

// Variable shadowing in nested scopes
defvar shadowed = 1;
foreach i = 1...2 in {
  defvar shadowed = 2;  // Shadows outer variable
  def shadow_test#i : BaseClass {
    let value = shadowed;
  }
}

// Defvar with conditional
defvar condition = 1;
defvar conditional_value = !if(condition, 100, 200);

// Variable shadowing inside if
defvar if_shadowed = 1;
if true then {
  defvar if_shadowed = 2;
}

// Variable shadowing inside let
defvar let_shadowed = 1;
let a = 1 in {
  defvar let_shadowed = 2;
}

// Defvar in multiclass
multiclass MulticlassWithVar<int base> {
  defvar offset = 10;
  def _inst : BaseClass {
    let value = !add(base, offset);
  }
}

defm var_multiclass : MulticlassWithVar<50>;

// Foreach iterator in defset
defset list<BaseClass> foreach_set = {
  foreach i = 1...3 in {
    def set_item#i : BaseClass {
      let value = i;
    }
  }
}

// Defvar with filter operation
defvar filter_list = !filter(x, [false, true], x);

// Defvar with foldl operation
defvar foldl_list = !foldl(0, [1, 2, 3, 4], acc, var, !add(acc, var));

// Defvar with foreach operation
defvar foreach_list = !foreach(x, [1, 2, 3, 4], !add(x, 1));
