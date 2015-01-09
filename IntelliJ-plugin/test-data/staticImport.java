package test;

import java.util.List;
import static java.util.Arrays.asList;

class X {
  void f() {
    X[] a = new X[5];
    List<X> x = asList(a)<caret>
  }
}