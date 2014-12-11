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
  
  private static float deleteCost(char[] meant, int i, char[] typed, int j) {
    return StringMatching.deleteCostConst();
  }

  private static float replaceCost(char[] meant, int i, char[] typed, int j, boolean lookahead) {
    char cm = meant[i];
    char ct = typed[j];
    char cn, cp;
    if (lookahead && i+1 < meant.length) 
      cn = meant[i+1]; 
    else 
      cn = cm;
    if (j > 0)
      cp = typed[j-1]; 
    else
      cp = cn;
    return StringMatching.replaceShiftCost(Character.isUpperCase(cp), Character.isUpperCase(ct), Character.isUpperCase(cm), Character.isUpperCase(cn))
         + StringMatching.charDistance(cm,ct);
  }

  private static float swapCost(char[] meant, int i, char[] typed, int j) {
    // cost for swapping i, i+1 to j, j+1
    // what would the cost be if we hadn't swapped?

    // swap i and i+1
    char tmp = meant[i];
    meant[i] = meant[i+1];
    meant[i+1] = tmp;

    float cost = StringMatching.swapCostConst() + replaceCost(meant, i, typed, j, true) + replaceCost(meant, i+1, typed, j+1, false);

    // swap back
    tmp = meant[i];
    meant[i] = meant[i+1];
    meant[i+1] = tmp;

    return cost;
  }

  private static float insertCost(char[] meant, int i, char[] typed, int j) {
    // accidentally hit a key next to the key before or after?
    char ca = meant[i]; // the key we mean to press
    char ci = typed[j]; // this is the key we're inserting (accidentally)

    if (j == 0) {
      return StringMatching.doubleTypeCost(Math.max(1.0f, StringMatching.charDistance(ca, ci)));
    } else {
      char cb = typed[j-1]; // the key we just pressed
      return StringMatching.insertShiftCost(Character.isUpperCase(cb), Character.isUpperCase(ci), Character.isUpperCase(ca))
           + StringMatching.doubleTypeCost(Math.min(Math.max(StringMatching.charDistance(cb, ci),1.0f), Math.max(1.0f,StringMatching.charDistance(ca, ci))));
    }
  }

  private static float[][] d = new float[10][10];
  public static float levenshteinDistance(char[] meant, int meant_length, char[] typed, int typed_length) {
    // make sure we have enough space
    if (typed_length+1 > d.length || meant_length+1 > d[0].length)
      d = new float[Math.max(meant_length+1,d.length)][Math.max(typed.length+1,d[0].length)];

    // d(i,j) is cost to obtain the first i character of meant having used the first j characters of typed

    // fill first column (moving down equals deletion of a character in meant
    for (int i = 0; i <= meant_length; ++i) {
      d[i][0] = i * StringMatching.deleteCostConst(); // d[i-1][0] + deleteCost(meant, i-1, "", 0);
    }

    // fill first row (moving right equals inserting a character into typed)
    for (int i = 1; i <= typed_length; ++i) {
      d[0][i] = d[0][i-1] + insertCost(meant, 0, typed, i-1);
    }

    for (int i = 1; i <= meant_length; ++i) {
      for (int j = 1; j <= typed_length; ++j) {
        // we're mentally at character i of what we intended to write, and we have already written j characters
        float del = d[i-1][j] + StringMatching.deleteCostConst(); // deleteCost(meant, i-1, typed, j-1), // omit a character of what we intended to write
        float ins = d[i][j-1] + insertCost(meant, i-1, typed, j-1); // insert a character typed[j-1] accidentally (without advancing our mental state of where we are with typing)
        float rep = d[i-1][j-1] + replaceCost(meant, i-1, typed, j-1, meant_length > i); // type a character (maybe getting it wrong)
        d[i][j] = Math.min(Math.min(del, ins), rep);
        // swapped two characters?
        if (j > 1 && i > 1) {
          float swp = d[i-1][j-2] + swapCost(meant, i-2, typed, j-2);
          d[i][j] = Math.min(d[i][j], swp);
        }

        // TODO: three subsequent replace actions are cheaper especially if the letters scrambled are on opposite
        // sides of the keyboard (synchronization between hands is hard)
      }
    }

    if (false && meant_length > 0) {
      String ms = String.copyValueOf(meant,0,meant_length);
      String ts = String.copyValueOf(typed,0,typed_length);
      float ld = StringMatching.levenshteinDistance(ms, ts);
      if (d[meant_length][typed_length] != ld)
        System.out.println("d(" + ms + ", " + ts + "): " + d[meant_length][typed_length] + ", ld: " + ld);
    }

    return d[meant_length][typed_length];
  }

  public static <V extends Tries.Named> scala.collection.immutable.List<Scores.Alt<V>> levenshteinLookup(Tries.Trie<V> t, String query, float maxDistance, double expected, double minProb) {
    List<Scores.Alt<V>> result = new SmartList<Scores.Alt<V>>();

    // convert typed string to array of int to avoid dealing with string allocations all the time
    char[] typed = query.toCharArray();
    int typed_length = typed.length;

    int[] s = t.structure();

    // allocate enough space for at least the query
    List<TriePos> pos = new ArrayList<TriePos>();
    pos.add(new TriePos(typed_length, s, 0));

    // plan for at least this much, increase as needed
    char[] prefix = new char[typed_length];
    int level = 0;

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

        // make sure we have prefix space to work with
        if (level >= prefix.length) {
          char[] newprefix = new char[(int)(1.5*(level+1))];
          System.arraycopy(prefix,0,newprefix,0,prefix.length);
          prefix = newprefix;
        }
                
        // next char
        char c = current.current(s);
        prefix[level] = c;

        // compute distance array in childPos and fill in distance and min_distance
        childPos.d[0] = current.d[0] + StringMatching.deleteCostConst(); //deleteCost(prefix, level, empty, 0);
        for (int j = 1; j <= typed_length; ++j) {
          float del = current.d[j] + StringMatching.deleteCostConst(); // deleteCost(prefix, level, typed, j - 1); // omit a character of what we intended to write
          float ins = childPos.d[j-1] + insertCost(prefix, level, typed, j - 1); // insert a character typed[j-1] accidentally (without advancing our mental state of where we are with typing)
          float rep = current.d[j-1] + replaceCost(prefix, level, typed, j - 1, false); // type a character (maybe getting it wrong)
          childPos.d[j] = Math.min(Math.min(del, ins), rep);
          if (j > 1 && last != null) {
            float swp = last.d[j-2] + swapCost(prefix, level - 1, typed, j - 2); // swapped two characters
            childPos.d[j] = Math.min(childPos.d[j], swp);
          }
        }

        childPos.distance = childPos.d[typed_length];
        childPos.min_distance = Float.MAX_VALUE;
        for (int i = 0; i <= typed_length; ++i) {
          childPos.min_distance = Math.min(childPos.min_distance, childPos.d[i]);
          if (i < typed_length-1) {
            childPos.min_distance = Math.min(childPos.min_distance, current.d[i] + StringMatching.minSwapCost());
          }
        }

        // descend into child if bound ok
        if (childPos.min_distance <= maxDistance) {
          current.descend(childPos, s);
          level++;
        }
      } else {
        // add this node's values
        if (current.distance <= maxDistance) {
          scala.collection.mutable.IndexedSeqView<V,V[]> values = t.nodeValues(current.node_idx);
          if (values.nonEmpty()) {
            double d = levenshteinDistance(prefix, level, typed, typed.length);
            double p = tarski.Pr.poissonPDF(expected, (int) Math.ceil(d));
            if (p >= minProb) {
              int l = values.length();
              for (int i = 0; i < l; ++i) {
                result.add(new tarski.Scores.Alt<V>(p, values.apply(i)));
              }
            }
          }
        }
        // pop this node
        level--;
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

    public TriePos(int typed_length) {
      d = new float[typed_length+1];
    }

    public TriePos(int typed_length, int[] structure, int node) {
      this(typed_length);
      node_idx = node;
      n_children = structure[node_idx+1];
      child = -1;

      // init distance for empty string
      for (int i = 0; i <= typed_length; ++i)
        d[i] = i * StringMatching.minInsertCost();
      min_distance = 0.f;
      distance = d[typed_length];
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

    public void descend(TriePos pos, int[] structure) {
      pos.node_idx = structure[node_idx+2+2*child+1];
      pos.n_children = structure[pos.node_idx+1];
      pos.child = -1;
    }
  }
}
