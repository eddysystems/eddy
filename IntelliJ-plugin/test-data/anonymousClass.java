class Test {
  void f(Runnable R) {}
  void g() {
    f(new Runnable() {
      public void test() {}

      @Override public void run() {
        true ? test() : this.test() <caret>
      }
    });
  }
}
