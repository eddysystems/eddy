
class ConstructorTest {

  class A {
    // this constructor is hidden
    private A() {}
  }

  class B extends A {
    public B(int i) {}

    public void f() {
      new A(); // illegal
      new B(); // illegal
      new C(); // legal

      blah<caret>
    }
  }

  class C {
    // this class should have an implicit constructor without arguments
  }
}