package test;

class A extends test2.X {}

class B extends A {
  class C {
    C(double d) {
    <caret>}
  }
}

