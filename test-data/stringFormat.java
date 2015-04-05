package test;

import java.util.Locale;

class Test {
  void f(String s, float f) {
    s = String.format(Locale.US, "%f", f)<caret>
  }
}