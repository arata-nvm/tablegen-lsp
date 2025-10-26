class Foo;

def foo: Foo;
def: Foo;

foreach i = 1...2 in
  def foo#i: Foo {
    int a = 1;
  }

defset list<Foo> foos = {
  def foo3: Foo;
  def foo4: Foo;
}