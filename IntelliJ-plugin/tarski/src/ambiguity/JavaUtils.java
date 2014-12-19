package ambiguity;

import gnu.trove.TObjectIntHashMap;
import org.apache.commons.lang.StringUtils;
import scala.collection.immutable.List;
import tarski.Items.*;
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

  public static void pushScope(String name) {
    final Scope s = scopes.peek();
    if (s.leaf) {
      System.out.printf("%s%s\n",StringUtils.repeat("  ",scopes.size()-2),s.name);
      s.leaf = false;
    }
    scopes.push(new Scope(name));
  }

  // IMPORTANT: Always call popScope as finally { popScope() } so that grep confirms safety
  public static void popScope() {
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

  public static <V extends Item> Map<TypeItem,Value[]> valuesByItem(V[] vs) {
    pushScope("values by item");
    try {
      // Turn debugging on or off
      final boolean debug = false;

      // Helpers for superItems loops
      final Stack<RefTypeItem> work = new Stack<RefTypeItem>();
      final Set<TypeItem> seen = new HashSet<TypeItem>();

      // Count values corresponding to each item
      final TObjectIntHashMap<TypeItem> count = new TObjectIntHashMap<TypeItem>(vs.length);
      for (final Item i : vs) {
        if (!(i instanceof Value))
          continue;
        final Value v = (Value)i;
        final TypeItem t = v.item();
        if (t instanceof LangTypeItem)
          count.put(t,count.get(t)+1);
        else {
          seen.clear();
          work.clear();
          work.push((RefTypeItem)t);
          while (!work.isEmpty()) {
            final RefTypeItem s = work.pop();
            if (s == ObjectItem$.MODULE$)
              continue;
            count.put(s,count.get(s)+1);
            List<RefTypeItem> ss = s.superItems();
            while (!ss.isEmpty()) {
              final RefTypeItem h = ss.head();
              if (!seen.contains(h)) {
                seen.add(h);
                work.push(h);
              }
              ss = (List<RefTypeItem>)ss.tail();
            }
          }
        }
      }

      // Add values corresponding to each item
      final Map<TypeItem,Value[]> results = new HashMap<TypeItem,Value[]>(count.size());
      for (final Item i : vs) {
        if (!(i instanceof Value))
          continue;
        final Value v = (Value)i;
        final TypeItem t = v.item();
        if (t instanceof LangTypeItem) {
          final int n = count.get(t);
          Value[] va = results.get(t);
          if (va == null) {
            va = new Value[n];
            results.put(t,va);
          }
          va[n-1] = v;
          count.put(t,n-1);
        } else {
          seen.clear();
          work.clear();
          work.push((RefTypeItem)t);
          while (!work.isEmpty()) {
            final RefTypeItem s = work.pop();
            if (s == ObjectItem$.MODULE$)
              continue;
            final int n = count.get(s);
            Value[] va = results.get(s);
            if (va == null) {
              va = new Value[n];
              results.put(s,va);
            }
            va[n-1] = v;
            count.put(s,n-1);
            List<RefTypeItem> ss = s.superItems();
            while (!ss.isEmpty()) {
              final RefTypeItem h = ss.head();
              if (!seen.contains(h)) {
                seen.add(h);
                work.push(h);
              }
              ss = (List<RefTypeItem>)ss.tail();
            }
          }
        }
      }

      // In debug mode, check that counts are exactly zero
      if (debug) {
        for (Object o : count.keys()) {
          TypeItem t = (TypeItem)o;
          int n = count.get(t);
          assert n==0 : "Bad count: t "+t+", n "+n;
        }
      }

      // All done!
      return results;
    } finally { popScope(); }
  }
}
