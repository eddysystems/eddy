package tarski;

import java.util.HashMap;
import java.util.Map;
import tarski.Items.*;

public final class ImportTrie {
  private static final class Node {
    int count = 0;
    int wilds = 0;
    Map<String,Node> kids = null;

    Node add(final String name) {
      if (kids == null)
        kids = new HashMap<String,Node>();
      Node n = kids.get(name);
      if (n == null) {
        n = new Node();
        kids.put(name,n);
      }
      n.count++;
      return n;
    }
  }
  private final Node root = new Node();

  // Add a qualified name representing an import
  public void add(final String qual) {
    final String[] names = qual.split("\\.");
    Node n = root;
    for (final String name : names) {
      if (name.equals("*"))
        n.wilds++;
      else
        n = n.add(name);
    }
  }

  // Standard imports
  public void addDefaults() {
    add("java.lang.*");
    add("java.lang.System.out");
    add("java.lang.Math");
  }

  // Pack and unpack info values
  static public long pack(final int size, final int prefix) { return (long)size<<32 | prefix; }
  static public int unpackSize(final long pack) { return (int)(pack>>32); }
  static public int unpackPrefix(final long pack) { return (int)pack; }

  // (n0<<32)|n1, where n0 is the length of item.qualified and n1 is the size of the imported prefix.
  // 0 for anonymous items (which cannot be imported).  The last skip components of the name are ignored.
  // WARNING: Not thread safe
  public final long info(Item item, final int skip) {
    // Collect names making up the qualified name, in reverse order
    int count = 0;
    for (int i=0;i<stack.length;i++) {
      stack[count++] = item.name();
      if (item instanceof Member)
        item = ((Member)item).parent();
      else if (item instanceof RootPackage)
        break;
      else
        return 0; // Anonymous
    }
    if (count == stack.length)
      return 0;
    final int size = count-skip;

    // Count how many levels of the qualified name were imported somewhere
    Node node = root;
    for (int k=count-1;k>=skip;k--) {
      if (node.kids != null) {
        final Node next = node.kids.get(stack[k]);
        if (next != null) {
          node = next;
          continue;
        }
      }
      return pack(size,count-k-1+(node.wilds!=0?1:0));
    }
    return pack(size,size);
  }

  // Private workspace for depth().  WARNING: Not thread safe.
  private final String[] stack = new String[100];
}
