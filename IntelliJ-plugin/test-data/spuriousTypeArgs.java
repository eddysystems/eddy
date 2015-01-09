package test;

import java.util.Map;
import java.util.HashMap;

class Test {
  static class X {}
  static class Y {}

  static class A {
    static <V> Map<X,V> f(V v) {
      return new HashMap<X,V>();
    }
  }

  void f() {
    Y y = null;
    Map<X,Y> map = A.f(y);<caret>
  }
}