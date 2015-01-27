class Test {
  static class A {
    static class B {
    }
  }
  void f() {
    if (true)
      A().B y;<caret>
  }
}
