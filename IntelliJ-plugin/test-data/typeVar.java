package test;

class D<Cvar> {}

class C<Avar> {
  int x;
  public <Bvar> void f() {
    x<caret>
  }
}
