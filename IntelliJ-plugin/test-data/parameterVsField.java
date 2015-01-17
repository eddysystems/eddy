class Test {
  class A {
    int x;  
  }
  static final A a;
  void f(int x) {}
  void g(int x) {
    f(x) <caret>
  }
}
