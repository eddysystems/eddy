package test;

import java.util.ArrayList;

class Test {
  static class XList extends ArrayList<Integer> {
    void dropFirst() {
      // make sure protected members of superclass are visible here
      removeRange(0,1)<caret>
    }
  }
}