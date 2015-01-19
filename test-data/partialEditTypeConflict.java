package test;

import java.util.List;
import java.util.ArrayList;

class OldOldOldType {}
class NewNewNewType {}

class A {
  int x;
  public void f() {
    List<NewNewNewType<caret>> xs = new ArrayList<OldOldOldType>();
  }
}
