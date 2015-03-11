abstract class X {
  public void x();

  static X makeX() {
    r = new X()<caret> {
      public void x() {}
    }

    return r;
  }
}