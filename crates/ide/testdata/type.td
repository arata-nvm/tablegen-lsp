// Basic types
class BasicTypes {
  bit bit_field;
  int int_field;
  string string_field;
  code code_field;
  dag dag_field;
}

// Bits type
class BitsTypes {
  bits<8> byte_field;
  bits<16> word_field;
  bits<32> dword_field;
}

// List types
class ListTypes {
  list<int> int_list;
  list<string> string_list;
  list<bit> bit_list;
}

// Nested list types
class NestedListTypes {
  list<list<int>> matrix;
}

// Record types
class RecordType;
class HasRecordType {
  RecordType record_field;
}

// Template argument type checking
class TypedTemplate<int i, string s, bit b> {
  int int_param = i;
  string string_param = s;
  bit bit_param = b;
}

// Correct usage
def correct_types : TypedTemplate<42, "hello", 1>;

// Field type compatibility
class FieldTypeCompat {
  int int_field = 10;
  string string_field = "test";
  bit bit_field = 0;
}

def field_compat_test : FieldTypeCompat {
  let int_field = 20;        // Compatible: int = int
  let string_field = "new";  // Compatible: string = string
  let bit_field = 1;         // Compatible: bit = bit
}

// List element type checking
class ListElementTypes {
  list<int> numbers = [1, 2, 3];
  list<string> strings = ["a", "b", "c"];
}

// Bits operations
class BitsOps {
  bits<8> value = 0xFF;
  bit single_bit = value{0};  // Bit extraction
  bits<4> nibble = value{3-0}; // Slice
}

// Type casting scenarios
class Base;
class Derived : Base;

class TypeCasting {
  Base base_field;
}

def derived_instance : Derived;

def cast_test : TypeCasting {
  let base_field = derived_instance;  // Derived can be assigned to Base
}

// Template with list type
class ListTemplate<list<int> values> {
  list<int> data = values;
}

def list_template_inst : ListTemplate<[1, 2, 3]>;

// Resolving common base type
class base;
class derived1 : base;
class derived2 : base;
def base_val : base;
def derived1_val : derived1;
def derived2_val : derived2;

defvar if1 = !if(false, base_val, base_val);
defvar if2 = !if(false, base_val, derived1_val);
defvar if3 = !if(false, derived1_val, base_val);
defvar if4 = !if(false, derived1_val, derived2_val);
defvar if5 = !if(false, derived2_val, derived1_val);

defvar listconcat1 = !listconcat([base_val], [base_val]);
defvar listconcat2 = !listconcat([base_val], [derived1_val]);
defvar listconcat3 = !listconcat([derived1_val], [base_val]);
defvar listconcat4 = !listconcat([derived1_val], [derived2_val]);
defvar listconcat5 = !listconcat([derived2_val], [derived1_val]);

class base1;
class base2;
class base3;
class derived12: base1, base2;
class derived23: base2, base3;
class derived123: base1, base2, base3;
def derived12_val : derived12;
def derived23_val : derived23;
def derived123_val : derived123;
defvar if_multi1 = !if(false, derived12_val, derived123_val);
defvar if_multi2 = !if(false, derived23_val, derived123_val);
defvar if_multi3 = !if(false, if_multi1, if_multi2);

// Range list type checking
class RangeListTest {
  bits<4> bits_field = 0;
  let bits_field{3...0} = 1;
  let bits_field{3...0, 3} = 1;
  let bits_field{8} = 1;

  string str_field = "hoge";
  let str_field{0} = "fuga";
}

// Suffix type checking
class SuffixTypeTest {
  bits<4> bits_field = 0xA;
  bit range_suffix_1 = bits_field{0};
  bits<2> range_suffix_2 = bits_field{3...2};

  list<int> int_list = [1, 2, 3];
  int slice_suffix_1 = int_list[0];
  list<int> slice_suffix_2 = int_list[0...2];
}
