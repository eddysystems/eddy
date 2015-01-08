package ambiguity;

import org.apache.commons.lang.StringUtils;
import scala.collection.immutable.Nil$;
import scala.collection.immutable.$colon$colon$;
import java.util.*;

public class JavaUtils {

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
}
