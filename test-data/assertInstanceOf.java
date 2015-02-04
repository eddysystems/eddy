
import java.util.List;

class Test {
  void f(Object elem) {
    assert elem instanceof List<caret>
  }
}