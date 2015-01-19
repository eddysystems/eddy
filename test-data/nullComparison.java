package test;

class Test {
  interface X {}
}

class Test2 {
  void f() {
    Test.X x = null;
    if (x != null) {}<caret>
  }
}