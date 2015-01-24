package test;

class X { // top-level, static

  // nothing in here would be in scope anyway, and this is only about scope

}

class Test { // top-level, static

  class Y {} // inner class, not static (but in scope: We can still say Y y, just new() expression must be qualified)

  static class Z {} // static nested class

  enum E {E1,E2} // enum, static

  interface I {} // interface, static

  static int static_i; // static field
  int i; // non-static field

  static void static_f() {} // static method
  void f() {} // non-static method

  static void test() {
    // local class, not static but in scope because static scope starts above here
    class Local { // not static, but in scope because static scope starts later, and we should have This and Super for it
      int local_i; // not static, but in scope because static scope starts later
      void test2() {
        <caret>
      }
    }
  }
}
