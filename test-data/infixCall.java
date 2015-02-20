import java.util.List;

class X {
  List<X> y;
  List<Integer> x;

  void f() {
    // if (y contains null) return; works
    // if (x.contains 0) return; works
    if (x contains 0) return;<caret>
  }
}