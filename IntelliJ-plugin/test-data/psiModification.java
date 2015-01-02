package test2;

public class Test {

  public static class Super {
    void Super(boolean b) {}
    Super(double x) {}
    f(int x) {}
  }

  public interface Interface {}

  public static class Sub
    <caret>extends Super
    implements Interface
  {}

  public static Super sup;
  public static Sub sub;
}