package ambiguity;

import com.intellij.util.SmartList;
import gnu.trove.TObjectIntHashMap;
import org.apache.commons.lang.StringUtils;
import tarski.Items.*;
import tarski.Pr;
import tarski.Scores.Alt;
import tarski.StringMatching;
import tarski.StringMatching.EmptyIncrementalLevenshteinBound$;
import tarski.StringMatching.IncrementalDistance;
import tarski.StringMatching.IncrementalLevenshteinBound;
import tarski.Tries.Named;
import tarski.Tries.Trie;

import java.util.*;

public class JavaUtils {
  // State for pushScope and popScope
  private static class Scope {
    final String name;
    final long start;
    boolean leaf;

    Scope(String name) {
      this.name = name;
      this.start = System.nanoTime();
      leaf = true;
    }
  }
  private static Stack<Scope> scopes;

  public static void pushScope(String name) {
    if (scopes == null) {
      scopes = new Stack<Scope>();
      final Scope top = new Scope("top");
      top.leaf = false;
      scopes.push(top);
    }
    final Scope s = scopes.peek();
    if (s.leaf) {
      System.out.printf("%s%s\n",StringUtils.repeat("  ",scopes.size()-2),s.name);
      s.leaf = false;
    }
    scopes.push(new Scope(name));
  }
  public static void popScope() {
    final long end = System.nanoTime();
    final Scope s = scopes.pop();
    System.out.printf("%s%s = %.3g s\n",StringUtils.repeat("  ",scopes.size()-1),s.name,1e-9*(end-s.start));
  }
  public static void popPushScope(String name) {
    popScope();
    pushScope(name);
  }

  // Size of common prefix of two strings
  public static int common(String x, String y) {
    int n = Math.min(x.length(),y.length());
    int p = 0;
    while (p < n && x.charAt(p) == y.charAt(p))
      p++;
    return p;
  }

  public static Map<TypeItem,Value[]> valuesByItem(Item[] vs) {
    pushScope("values by item");
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
          scala.collection.immutable.List<RefTypeItem> ss = s.superItems();
          while (!ss.isEmpty()) {
            final RefTypeItem h = ss.head();
            if (!seen.contains(h)) {
              seen.add(h);
              work.push(h);
            }
            ss = (scala.collection.immutable.List<RefTypeItem>)ss.tail();
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
          scala.collection.immutable.List<RefTypeItem> ss = s.superItems();
          while (!ss.isEmpty()) {
            final RefTypeItem h = ss.head();
            if (!seen.contains(h)) {
              seen.add(h);
              work.push(h);
            }
            ss = (scala.collection.immutable.List<RefTypeItem>)ss.tail();
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
    popScope();
    return results;
  }

}
