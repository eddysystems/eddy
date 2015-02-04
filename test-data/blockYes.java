class Test {
  static class A {
    static class B {
    }
  }
  void g();
  void h();

  void f() {
    if (true)
      true ? g() : 0;<caret>
    else
      h();
  }
}
