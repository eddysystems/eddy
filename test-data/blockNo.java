class Test {
  static class A {
    static class B {
    }
  }
  void g();

  int f() {
    true ? g() : 0;<caret>
  }
}
