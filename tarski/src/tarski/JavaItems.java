package tarski;

import tarski.Items.*;
import scala.collection.immutable.List;
import gnu.trove.TObjectIntHashMap;
import java.util.*;

public class JavaItems {

  public static Map<TypeItem, Value[]> valuesByItem(Map<Item,Integer> vs) {
    int n = vs.size();
    Item[] a = new Item[n];
    n = 0;
    for (Map.Entry<Item,Integer> e : vs.entrySet()) {
      a[n++] = e.getKey();
    }
    return valuesByItem(a);
  }

  public static Map<TypeItem, Value[]> valuesByItem(java.util.List<Item> vs) {
    return valuesByItem(vs.toArray(new Item[vs.size()]));
  }

  public static <V extends Item> Map<TypeItem,Value[]> valuesByItem(V[] vs) {
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
  }
}
