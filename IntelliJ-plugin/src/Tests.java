import org.testng.annotations.BeforeClass;

public class Tests {
  @BeforeClass
  public void init() {
    // read test data from file or do anything only once
  }

  public static String getClassName(int t) {
    return "int";
  }

  public static String getClassName(double d) {
    return "double";
  }

  public static String getClassName(boolean b) {
    return "boolean";
  }

  public static String getClassName(char c) {
    return "char";
  }

  public static String getClassName(byte b) {
    return "byte";
  }

  public static String getClassName(short s) {
    return "short";
  }

  public static String getClassName(long l) {
    return "long";
  }

  public static String getClassName(float f) {
    return "float";
  }

  public static <T> String getClassName(T t) {
    return t.getClass().getCanonicalName();
  }

  public static void test() {
  }

  /*
  @Test
  public void expressionTypes() {

    byte bt = (byte)220;
    short sh = (short)40000;
    int i = 4;
    long l = 500000000l;

    float f = 1.0f;
    double d = 1.0;

    List<Number> args = new SmartList<Number>(i, d, f, l, sh, bt);
    List<BinaryOp> ops = new SmartList<BinaryOp>(new AddOp(), new MulOp(), new RShiftOp(), new LtOp(), new EqOp(), new AndOp(), new AndAndOp());

    for (BinaryOp op : ops) {
      for (Number t1: args) {
        for (Number t2: args) {
          Option<Type> type = tarski.Types$.MODULE$.binaryType(op, t1, t2);
          if (type.isDefined()) {
            if (op instanceof AddOp) {
              assert getClassName(t1 + t2) == type.get.qualifiedName;
            }
          }
        }
      }
    }
  }
  */

  /* add tests like so
  @Test
  public void tokenize() {

    // output in here shows up in nice logs
    System.out.println("tokenize test");
  }
  */
}

