
class ConstructorTest {

  class TestClassA$ {
    // this constructor is hidden
    private TestClassA$() {}
  }

  class TestClassB$ extends TestClassA$ {
    public TestClassB$(int i) {}

    public void f() {
      new TestClassA$(); // illegal
      new TestClassB$(); // illegal
      new TestClassC$(); // legal

      blah<caret>
    }
  }

  class TestClassC$ {
    // this class should have an implicit constructor without arguments
  }
}