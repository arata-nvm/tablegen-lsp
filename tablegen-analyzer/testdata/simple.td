// This is a document.
class Foo<int a> {
  int A = a;
}
class Bar : Foo<1>;

defset list<Foo> foos {
  def foo1: Foo<2>;
  def foo2: Bar;
}

// document 2
def foo: Foo<3>;
