class Outer {
  static class Inner {
    int x;
    static void f() {
      x = 7 <caret>
    }
  }
}
