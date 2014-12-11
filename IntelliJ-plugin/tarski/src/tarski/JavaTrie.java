package tarski;

import ambiguity.JavaUtils;
import com.intellij.util.SmartList;

import java.util.ArrayList;
import java.util.List;

public class JavaTrie {

  // Should be parameterized over V extends Named.  That causes weird build issues, so we hard code V = Named.
  public static int[] makeTrieStructure(Tries.Named[] values) {
    JavaUtils.pushScope("trie structure");
    // Count nodes and determine maximum depth
    //      : *0-         : 1,3
    // a    : *1a#*0-     : 2,7
    // a b  : *2a#b#*0*0- : 3,11
    // a ab : *1a#*1b#*0- : 3,11
    JavaUtils.pushScope("count");
    int nodes = 1;
    int maxSize = 0;
    String prev = "";
    for (int i = 0; i < values.length; ++i) {
      String k = values[i].name();
      int kl = k.length();
      maxSize = Math.max(maxSize, kl);
      nodes += kl - JavaUtils.common(prev, k);
      prev = k;
    }
    int depth = maxSize + 1;
    int structureSize = 4*nodes-1;

    // Determine node information: an array of (position,start) pairs.
    JavaUtils.popPushScope("allocate info+stack");
    int[] info = new int[2*nodes+1];
    int[] stack = new int[depth];

    // At first, each info pair is (children,start)
    JavaUtils.popPushScope("children");
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

    // Accumulate children into position
    JavaUtils.popPushScope("position");
    int total = 0;
    for (n = 0; n < nodes; ++n) {
      int next = total+2+2*info[2*n];
      info[2*n] = total;
      total = next;
    }
    assert(total+1 == structureSize);
    info[2*nodes] = total;

    // Allocate structure
    JavaUtils.popPushScope("allocate structure");
    int[] structure = new int[structureSize];

    // Generate tree
    // Initialize value starts.  Child counts are correctly already zero.
    JavaUtils.popPushScope("structure");
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

    JavaUtils.popScope();
    JavaUtils.popScope();
    return structure;
  }

  public static <V extends Tries.Named> scala.collection.immutable.List<Scores.Alt<V>> levenshteinLookup(Tries.Trie<V> t, String typed, float maxDistance, double expected, double minProb) {
    List<Scores.Alt<V>> result = new SmartList<Scores.Alt<V>>();

    int[] s = t.structure();

    List<TriePos> pos = new ArrayList<TriePos>();
    int typed_length = typed.length();
    pos.add(new TriePos(typed_length, s, 0, tarski.StringMatching.EmptyIncrementalLevenshteinBound$.MODULE$));

    String prefix = "";
    int level = 0;

    System.out.println("finding " + typed);

    while (level >= 0) {
      TriePos current = pos.get(level);

      // while there are still children to traverse
      if (current.next()) {
        // get two ago for char swap if available
        TriePos last = (level > 0 ? pos.get(level-1) : null);
        // make sure we have child data to work with
        if (level+1 >= pos.size())
          pos.add(new TriePos(typed_length));
        TriePos childPos = pos.get(level+1);

        // next char
        char c = current.current(s);
        String newprefix = prefix + c;

        // compute distance array in childPos and fill in distance and min_distance
        StringMatching.IncrementalDistance cdist = new StringMatching.IncrementalLevenshteinBound(typed,current.dist,c);

        childPos.d[0] = current.d[0] + StringMatching.deleteCost(newprefix, level, "", 0);
        for (int j = 1; j <= typed_length; ++j) {
          float del = current.d[j] + StringMatching.deleteCost(newprefix, level, typed, j - 1); // omit a character of what we intended to write
          float ins = childPos.d[j-1] + StringMatching.insertCost(newprefix, level, typed, j - 1); // insert a character typed[j-1] accidentally (without advancing our mental state of where we are with typing)
          float rep = current.d[j-1] + StringMatching.replaceCost(newprefix, level, typed, j - 1); // type a character (maybe getting it wrong)
          childPos.d[j] = Math.min(Math.min(del, ins), rep);
          if (j > 1 && last != null) {
            float swp = last.d[j-2] + StringMatching.swapCost(newprefix, level - 1, typed, j - 2); // swapped two characters
            childPos.d[j] = Math.min(childPos.d[j], swp);
          }
        }

        childPos.distance = childPos.d[typed_length];
        childPos.min_distance = 0.f;
        for (int i = 0; i <= typed_length; ++i) {
          assert childPos.d[i] == cdist.d(i);

          childPos.min_distance = Math.min(childPos.min_distance, childPos.d[i]);
          if (i < typed_length-1) {
            childPos.min_distance = Math.min(childPos.min_distance, current.d[i] + StringMatching.minSwapCost());
          }
        }
        assert childPos.min_distance == cdist.min();
        assert childPos.distance == cdist.distance();

        // descend into child if bound ok
        if (childPos.min_distance <= maxDistance) {
          prefix = newprefix;
          current.descend(childPos, s, cdist);
          level++;
        }
      } else {
        // add this node's values
        if (current.dist.distance() <= maxDistance) {
          double d = StringMatching.levenshteinDistance(typed, current.dist.current());
          double p = tarski.Pr.poissonPDF(expected, (int) Math.ceil(d));
          if (p >= minProb) {
            scala.collection.mutable.IndexedSeqView<V,V[]> values = t.nodeValues(current.node_idx);
            int l = values.length();
            for (int i = 0; i < l; ++i) {
              result.add(new tarski.Scores.Alt<V>(p, values.apply(i)));
            }
          }
        }
        // pop this node
        level--;
        if (level >= 0)
          prefix = prefix.substring(0,level);
      }
    }

    return scala.collection.JavaConversions.asScalaBuffer(result).toList();
  }

  static class TriePos {
    public int node_idx; // start index of node in structure array
    public int n_children; // number of children
    public int child; // index of current child (in node's children array)

    // possible distance array
    public float[] d;
    public float min_distance, distance;

    public StringMatching.IncrementalDistance dist;

    public TriePos(int typed_length) {
      d = new float[typed_length+1];
    }

    public TriePos(int typed_length, int[] structure, int node, StringMatching.IncrementalDistance dist) {
      this(typed_length);
      node_idx = node;
      n_children = structure[node_idx+1];
      child = -1;

      // init distance for empty string
      for (int i = 0; i <= typed_length; ++i)
        d[i] = i * StringMatching.minInsertCost();
      min_distance = 0.f;
      distance = d[typed_length];

      this.dist = dist;
    }

    // to iterate over children:
    // TriePos childPos;
    // while (next()) {
    //   char c = current();
    //   ...
    //   descend(childPos...);
    //   ...
    // }

    public boolean next() {
      child++;
      return child < n_children;
    }

    char current(int[] structure) {
      return (char)structure[node_idx+2+2*child];
    }

    public void descend(TriePos pos, int[] structure, StringMatching.IncrementalDistance dist) {
      pos.node_idx = structure[node_idx+2+2*child+1];
      pos.n_children = structure[pos.node_idx+1];
      pos.child = -1;

      pos.dist = dist;
    }
  }
}
