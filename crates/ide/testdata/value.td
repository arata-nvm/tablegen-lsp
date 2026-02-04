class Foo {
    bits<4> x = 0b0000;
    let x{3-2} = 0b00;
}

class Interleave {
    string x = !interleave(["foo", "bar"], ",")
}

class Cond {
    int ok = !cond(true: 1, false: 2);
    int bad_cond = !cond("str": 1, false: 2);
    int bad_value = !cond(true: 1, true: "str");
}

class BitsConcat {
  bits<3> a = 0b001;
  bits<4> b = 0b0101;
  bits<7> ab = {a, b};
  bits<7> = {0b0001, 0b00, true};
}