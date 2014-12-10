package ambiguity;

import tarski.Tries.Named;

public class JavaUtils {
    // Size of common prefix of two strings
  public static int common(String x, String y) {
    int n = Math.min(x.length(),y.length());
    int p = 0;
    while (p < n && x.charAt(p) == y.charAt(p))
      p++;
    return p;
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
