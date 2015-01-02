package test2;

public class Test {

  public static class Super {}

  public interface Interface {}

  public static class Sub
    <caret>extends Super
    implements Interface
  {}

  public static Super sup;
  public static Sub sub;
}