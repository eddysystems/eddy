import java.util.Set;

class X {
  public boolean valid() { return true; };
}

class Y {
  void f(Set<X> set) {
    for x in set: if valid <caret>
  }
}