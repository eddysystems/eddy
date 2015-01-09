class Test {
  interface X {}
}

class Test2 {
  void f() {
    I x = null;
    if (x != null) {}<caret>
  }
}