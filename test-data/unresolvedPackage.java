class Test {
  static class XList extends List<Integer> {
    void f() {
      // make sure protected members of superclass are visible here
      int x<caret>
    }
  }
}