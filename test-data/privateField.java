package test;

class X {
  private X myManager;

  void f() {
    myManager = new X();<caret>
  }
}