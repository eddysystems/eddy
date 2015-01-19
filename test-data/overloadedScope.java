package test;

import static java.util.Arrays.*;

class X {
  void f() {
    int[] a = new int[10];
    fill(a, binarySearch(a, 5))<caret>
  }
}