class Test {
  static class B {
    public void bar() {}
  }

  static class A {
    public B foo() { return null; }
  }

  static void foo(final A x) {
    bar<caret>
  }
}
