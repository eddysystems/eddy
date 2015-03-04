package utility;

import org.apache.commons.lang.StringUtils;
import scala.collection.immutable.Nil$;
import scala.collection.immutable.$colon$colon$;
import java.util.*;

public class JavaUtils {
  public static boolean safeEquals(final Object x, final Object y) {
    return x==y || (x!=null && x.equals(y));
  }

  public static double poissonPDF(double lambda, int k) {
    // lambda^k/k! * e^-lambda
    float lk_kfac = 1;
    for (int i = 1; i <= k; ++i)
      lk_kfac *= lambda/i;
    return lk_kfac * Math.exp(-lambda);
  }

  // State for pushScope and popScope
  private static final class Scope {
    final String name;
    final long start;
    boolean leaf;

    Scope(String name) {
      this.name = name;
      this.start = System.nanoTime();
      leaf = true;
    }
  }
  private static Stack<Scope> initScopes() {
    final Stack<Scope> scopes = new Stack<Scope>();
    final Scope top = new Scope("top");
    top.leaf = false;
    scopes.push(top);
    return scopes;
  }
  private static final Stack<Scope> scopes = initScopes();
  public static boolean skipScopes = false; // Used to silence tiny irrelevant scopes

  public static void pushScope(String name) {
    if (skipScopes)
      return;
    final Scope s = scopes.peek();
    if (s.leaf) {
      System.out.printf("%s%s\n",StringUtils.repeat("  ",scopes.size()-2),s.name);
      s.leaf = false;
    }
    scopes.push(new Scope(name));
  }

  // IMPORTANT: Always call popScope as finally { popScope() } so that grep confirms safety
  public static void popScope() {
    if (skipScopes)
      return;
    assert scopes.size() > 1;
    final long end = System.nanoTime();
    final Scope s = scopes.pop();
    System.out.printf("%s%s = %.3g s\n",StringUtils.repeat("  ",scopes.size()-1),s.name,1e-9*(end-s.start));
  }

  // Size of common prefix of two strings
  public static int common(String x, String y) {
    int n = Math.min(x.length(),y.length());
    int p = 0;
    while (p < n && x.charAt(p) == y.charAt(p))
      p++;
    return p;
  }

  // Build a scala list from Java
  public static <A> scala.collection.immutable.List<A> scalaList(final A... xs) {
    scala.collection.immutable.List<A> r = (scala.collection.immutable.List)Nil$.MODULE$;
    for (int i=xs.length-1;i>=0;i--)
      r = $colon$colon$.MODULE$.apply(xs[i],r);
    return r;
  }

  // Are we in debug mode?
  private static int _isDebug = 2; // 0 for no, 1 for yes, 2 for uninitialized
  public static boolean isDebug() {
    if (_isDebug == 2) {
      // See https://stackoverflow.com/questions/3776204/how-to-find-out-if-debug-mode-is-enabled
      _isDebug = java.lang.management.ManagementFactory.getRuntimeMXBean()
                   .getInputArguments().toString().contains("jdwp") ? 1 : 0;
    }
    return _isDebug == 1;
  }

  // Push and pop temporary debug mode (for use in unit tests).  Always use popDebug in a finally block.
  private static Stack<Boolean> debugStack = new Stack<Boolean>();
  public static void pushDebug() {
    debugStack.push(isDebug());
    _isDebug = 1;
  }
  public static void popDebug() {
    _isDebug = debugStack.pop() ? 1 : 0;
  }

  // Concatenate a bunch of arrays
  public static <A> A[] concatenate(final A[]... arrays) {
    assert arrays.length > 0;
    int total = 0;
    for (int i=0;i<arrays.length;i++)
      total += arrays[i].length;
    final A[] all = (A[])java.lang.reflect.Array.newInstance(arrays[0].getClass().getComponentType(),total);
    int offset = 0;
    for (int i=0;i<arrays.length;i++) {
      final A[] a = arrays[i];
      System.arraycopy(a,0,all,offset,a.length);
      offset += a.length;
    }
    return all;
  }

  // Thomas Wang's 32 bit to 32 bit hash
  public static int hash(int k) {
    k = (k ^ 61) ^ (k >>> 16);
    k = k + (k << 3);
    k = k ^ (k >>> 4);
    k = k * 0x27d4eb2d; // TODO
    k = k ^ (k >>> 15);
    return k;
  }
}
