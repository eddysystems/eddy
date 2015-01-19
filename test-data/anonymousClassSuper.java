class Test {
  abstract class Base {
    final void foo() {}
    abstract void bar(); 
  }
  static void f(Base b) {}
  static void g() {
    f(new Base() {
      @Override public void bar() {
        super.foo() <caret>
      }
    });
  }
}
