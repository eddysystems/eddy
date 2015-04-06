class Test {
  void foo() {
    int x;
    class A {
      void bar() {
        boolean x = true <caret>
      }
    }
  }
}
