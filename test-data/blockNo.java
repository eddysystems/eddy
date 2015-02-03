class Test {
  static class A {
    static class B {
    }
  }
  void g();
  void h();

  int f() {
    true ? g() : h();<caret>
  }
}
