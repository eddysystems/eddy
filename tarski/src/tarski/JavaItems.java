/* JavaItems: Performance critical code for by-item precomputation and lookup */

package tarski;

import com.intellij.util.SmartList;
import tarski.Items.*;
import tarski.Types.*;
import tarski.Scores.*;
import java.util.*;

import static tarski.Flags.nullaryMethods;

public class JavaItems {

  public static final class ByItemMaps implements ValueByItemQuery {
    final public Map<TypeItem,Value[]> values;
    final public Map<TypeItem,MethodItem[]> methods;

    ByItemMaps(final Map<TypeItem,Value[]> values, final Map<TypeItem,MethodItem[]> methods) {
      this.values = values;
      this.methods = methods;
    }

    public Scored<ValueOrMethod> query(final TypeItem ty) {
      Scored<ValueOrMethod> s = (Scored)Empty$.MODULE$;
      if (nullaryMethods) {
        final MethodItem[] ms = methods.get(ty);
        if (ms != null)
          for (int i=0;i<ms.length;i++)
            s = new Best<ValueOrMethod>(Pr.methodByItem(),ms[i],s);
      }
      final Value[] vs = values.get(ty);
      if (vs != null)
        for (int i=0;i<vs.length;i++)
          s = new Best<ValueOrMethod>(JavaScores.one,vs[i],s);
      return s;
    }
  }

  public static ByItemMaps valuesByItem(final List<Item> vs, final boolean addObject) {
    return valuesByItem(vs.toArray(new Item[vs.size()]), addObject);
  }

  // Is a method suitable for by item lookup?
  // IMPORTANT: Must match ByItem.considerMethod
  static public boolean considerMethod(final MethodItem m, final TypeItem ret) {
    return ret != Base.ubVoidItem$.MODULE$ && m.tparams().isEmpty() && m.arity()==0;
  }

  public static <V extends Item> ByItemMaps valuesByItem(final V[] vs, final boolean addObject) {
    // Helpers for superItems loops
    final Stack<RefTypeItem> work = new Stack<RefTypeItem>();
    final Set<TypeItem> seen = new HashSet<TypeItem>();

    // We build into a temporary map first, then partition into separate values and methods maps later
    final Map<TypeItem,List<ValueOrMethod>> both = new HashMap<TypeItem,List<ValueOrMethod>>();
    for (final Item i : vs) {
      if (!(i instanceof ValueOrMethod) || i instanceof SuperItem) // Always use this instead of super
        continue;
      final ValueOrMethod v = (ValueOrMethod)i;
      final TypeItem t;
      if (i instanceof Value)
        t = ((Value)i).item();
      else if (nullaryMethods && i instanceof MethodItem) {
        final MethodItem m = (MethodItem)i;
        t = m.retItem();
        if (!considerMethod(m,t))
          continue;
      } else
        continue;
      if (t instanceof LangTypeItem) {
        List<ValueOrMethod> va = both.get(t);
        if (va == null) {
          va = new SmartList<ValueOrMethod>();
          both.put(t,va);
        }
        va.add(v);
      } else {
        seen.clear();
        work.clear();
        work.push((RefTypeItem)t);
        while (!work.isEmpty()) {
          final RefTypeItem s = work.pop();
          if (!addObject && s == ObjectItem$.MODULE$)
            continue;
          List<ValueOrMethod> va = both.get(s);
          if (va == null) {
            va = new SmartList<ValueOrMethod>();
            both.put(s, va);
          }
          va.add(v);
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

    // Split into separate maps for values and methods
    final Map<TypeItem, Value[]> values = new HashMap<TypeItem, Value[]>();
    final Map<TypeItem, MethodItem[]> methods = new HashMap<TypeItem, MethodItem[]>();
    for (final Map.Entry<TypeItem, List<ValueOrMethod>> e : both.entrySet()) {
      final TypeItem t = e.getKey();
      final List<ValueOrMethod> xs = e.getValue();
      final int nBoth = xs.size();
      int nValues = 0;
      for (int i=0;i<nBoth;i++)
        if (xs.get(i) instanceof Value)
          nValues++;
      int nMethods = nBoth - nValues;
      final Value[] va = nValues != 0 ? new Value[nValues] : null;
      final MethodItem[] ma = nMethods != 0 ? new MethodItem[nMethods] :  null;
      for (int i=0;i<nBoth;i++) {
        final ValueOrMethod x = xs.get(i);
        if (x instanceof Value) va[--nValues] = (Value)x;
        else ma[--nMethods] = (MethodItem)x;
      }
      if (va != null) values.put(t, va);
      if (ma != null) methods.put(t, ma);
    }

    // All done!
    return new ByItemMaps(values,methods);
  }
}
