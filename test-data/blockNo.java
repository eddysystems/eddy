class Test {
  static class A {
    static class B {
    }
  }
  void f() {
    A().B y;<caret>
  }
}
