package ambiguity;

import tarski.Items.*;
import tarski.Base.*;
import tarski.Tries.Named;
import gnu.trove.map.hash.TObjectIntHashMap;

import java.util.*;

public class JavaUtils {
    // Size of common prefix of two strings
  public static int common(String x, String y) {
    int n = Math.min(x.length(),y.length());
    int p = 0;
    while (p < n && x.charAt(p) == y.charAt(p))
      p++;
    return p;
  }

  public static Map<TypeItem,Value[]> valuesByItem(Map<TypeItem,Value[]> start, Item[] vs) {
    // Turn debugging on or off
    final boolean debug = false;

    // Helpers for superItems loops
    final Stack<RefTypeItem> work = new Stack<RefTypeItem>();
    final Set<TypeItem> seen = new HashSet<TypeItem>();

    // Initialize counts based on previous map
    final TObjectIntHashMap<TypeItem> count = new TObjectIntHashMap<TypeItem>(start.size());
    for (Map.Entry<TypeItem,Value[]> e : start.entrySet())
      count.put(e.getKey(),e.getValue().length);

    // Count values corresponding to each item
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

    // Allocate new map and initialize with old values.
    // If there are no new Value for a given TypeItem, we reuse the Value[].
    final Map<TypeItem,Value[]> results = new HashMap<TypeItem,Value[]>(start.size());
    for (final Map.Entry<TypeItem,Value[]> e : start.entrySet()) {
      final TypeItem t = e.getKey();
      int n = count.get(t);
      final Value[] vaOld = e.getValue();
      Value[] va;
      if (n == vaOld.length) {
        va = vaOld;
        if (debug)
          count.put(t,0);
      } else {
        va = results.get(t);
        if (va == null) {
          va = new Value[n];
          results.put(t,va);
        }
        for (int i=0;i<vaOld.length;i++)
          va[--n] = vaOld[i];
        count.put(t,n);
      }
      results.put(t,va);
    }

    // Add values corresponding to each item
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
    return results;
  }

  public static <V extends Named> int[] makeTrieStructure(V[] values) {
    // Count nodes and determine maximum depth
    //      : *0-         : 1,3
    // a    : *1a#*0-     : 2,7
    // a b  : *2a#b#*0*0- : 3,11
    // a ab : *1a#*1b#*0- : 3,11
    long start = System.nanoTime();
    int nodes = 1;
    int maxSize = 0;
    String prev = "";
    for (int i = 0; i < values.length; ++i) {
      String k = values[i].name();
      int kl = k.length();
      maxSize = Math.max(maxSize, kl);
      nodes += kl - JavaUtils.common(prev,k);
      prev = k;
    }
    int depth = maxSize + 1;
    int structureSize = 4*nodes-1;
    long end = System.nanoTime();
    System.out.println("elapsed count = "+(end-start)/1e9);


    // Determine node information: an array of (position,start) pairs.
    start = System.nanoTime();
    int[] info = new int[2*nodes+1];
    int[] stack = new int[depth];
    end = System.nanoTime();
    System.out.println("elapsed allocate info+stack = "+(end-start)/1e9);

    // At first, each info pair is (children,start)
    start = System.nanoTime();
    prev = "";
    int n = 1;
    for (int i = 0; i < values.length; ++i) {
      String k = values[i].name();
      int c = JavaUtils.common(prev,k); // Implicit truncate stack to size c+1
      if (c < k.length()) {
        info[2*stack[c]] += 1;
        for (int j = c+1; j < k.length(); ++j) {
          info[2*n] += 1;
          info[2*n+1] = i;
          stack[j] = n;
          n += 1;
        }
        info[2*n+1]= i;
        stack[k.length()] = n;
        n += 1;
      }
      prev = k;
    }
    assert n == nodes;
    end = System.nanoTime();
    System.out.println("elapsed children loop = "+(end-start)/1e9);


    // Accumulate children into position
    start = System.nanoTime();
    int total = 0;
    for (n = 0; n < nodes; ++n) {
      int next = total+2+2*info[2*n];
      info[2*n] = total;
      total = next;
    }
    assert(total+1 == structureSize);
    info[2*nodes] = total;
    end = System.nanoTime();
    System.out.println("elapsed position loop = "+(end-start)/1e9);

    // Allocate structure
    start = System.nanoTime();
    int[] structure = new int[structureSize];
    end = System.nanoTime();
    System.out.println("elapsed allocate structure = "+(end-start)/1e9);

    // Generate tree
    // Initialize value starts.  Child counts are correctly already zero.
    for (n = 0; n < nodes; ++n)
      structure[info[2*n]] = info[2*n+1];
    structure[info[2*nodes]] = values.length;
    // Fill in children
    // stack is already the right size and I don't need to reinitialize
    prev = "";
    n = 1;
    for (int i = 0; i < values.length; ++i) {
      String k = values[i].name();
      int kl = k.length();
      int c = JavaUtils.common(prev,k); // Implicit truncate stack to size c+1
      if (c < kl) {
        int pn = info[2*stack[c]];
        int cn = structure[pn+1];
        structure[pn+1] = cn+1;
        structure[pn+2+2*cn] = k.charAt(c);
        structure[pn+2+2*cn+1] = info[2*n];
        n++;

        for (int j = c+1; j < kl; ++j) {
          stack[j] = n-1;
          pn = info[2*(n-1)];
          cn = structure[pn+1];
          structure[pn+1] = cn+1;
          structure[pn+2+2*cn] = k.charAt(j);
          structure[pn+2+2*cn+1] = info[2*n];
          n++;
        }
        stack[kl] = n-1;
      }
      prev = k;
    }
    assert n == nodes;

    return structure;
  }

}
