package com.eddysystems.eddy;

import org.testng.annotations.BeforeClass;

class Z {
  int x;
}

class V {
  int x;
}

class X extends Z {
  int x;
  static final Integer q = 0;
  static int blah() { return 1; }
}

class S extends V {
  int x;

  class Y extends X {
    int x;

    public X getX() {
      return this;
    }

    public void t() {
      int x = 1;

      int a[];

      x = (getX()).blah();

      x *= 2; // local
      this.x *= 2; // Y.x
      super.x *= 2; // X.x
      ((X)this).x *= 2; // X.x
      ((Z)this).x *= 2; // Z.x
      S.this.x *= 2; // S.x
      S.super.x *= 2; // V.x

      class L {
        int x = Y.this.x;
      }
    }
  }

  Y y;
}

class T extends X {
  static int g;
  final static Integer q = 0;

  static class R {
    static class Q {
      final static Integer q = 0;

      public void test() {
        g = 2;
        this.hashCode();
      }
    }

    // no static fields here
  }

  public static void t() {
    final Integer q = 1;

    q.hashCode(); // local
    T.q.hashCode(); // T.q
    X.q.hashCode(); // X.q
  }
}

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

  public static float x(double d) {
    return (float)d;
  }

  public static void map() {
    Object x = x(1.1) > 0 ? true : new Tests();
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

