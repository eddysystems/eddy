import static java.lang.Runtime.*;

class X {
  void f() {
    getRuntime().gc()<caret>
  }
}