package ambiguity;

import gnu.trove.TObjectIntHashMap;
import org.apache.commons.lang.StringUtils;
import tarski.Items.*;
import tarski.Scores.*;
import scala.Function1;
import java.util.*;
import static java.lang.Math.max;

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

  public static <V extends Item> Map<TypeItem,Value[]> valuesByItem(V[] vs) {
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

  // Best(p,x,r).bias(q)
  static final class Biased<B> extends AltBase {
    final double _p;
    final B x;
    final LazyScored<B> r;
    final double _q;

    Biased(double p, B x, LazyScored<B> r, double q) {
      this._p = p;
      this.x = x;
      this.r = r;
      this._q = q;
    }

    public double p() {
      return _p*_q;
    }
  }

  static public final class FlatMapState<A,B> {
    public FlatMapState(Scored<A> input, Function1<A,Scored<B>> f) {
      this.as = input;
      this.f = f;
    }

    // Our flatMap function
    private final Function1<A,Scored<B>> f;

    // Exactly one of as and asLazy is null
    private Scored<A> as;
    private LazyScored<A> asLazy;

    // Sorted strict and unsorted lazy bs
    // The AltBase is either Biased<B> or Alt<LazyScored<B>>
    private PriorityQueue<AltBase> bs;

    // Grab the next as
    private Scored<A> nextAs() {
      if (as == null) {
        LazyScored<A> r = asLazy;
        asLazy = null;
        return r.s();
      } else {
        Scored<A> r = as;
        as = null;
        return r;
      }
    }

    // Our current probability bound
    private double p() {
      double p = as != null ? as.p() : asLazy.p();
      if (bs != null && !bs.isEmpty())
        p = max(p,bs.peek().p());
      return p;
    }

    // Invariant: This class will go out of scope after extract(), unless we do otherwise
    public Scored<B> extract() {
      for (;;) {
        // If bs is better than as, we're done
        if (bs != null) {
          while (!bs.isEmpty()) {
            final AltBase ab = bs.peek();
            if (ab instanceof Alt) {
              // The best bs is lazy, so force it and keep going
              bs.poll();
              Alt<LazyScored<B>> pb = (Alt)ab;
              Scored<B> _b = pb.x().s();
              if (_b instanceof Best) {
                final Best<B> b = (Best<B>)_b;
                bs.add(new Biased<B>(b.p(),b.x(),b.r(),pb.p()));
              }
            } else {
              final Biased<B> b = (Biased<B>)ab;
              final double bp = b.p();
              boolean done;
              if (as != null)
                done = bp >= as.p();
              else if (bp >= asLazy.p())
                done = true;
              else {
                as = asLazy.s();
                asLazy = null;
                done = bp >= as.p();
              }
              if (done) {
                bs.poll();
                bs.add(new Alt<LazyScored<B>>(b._q,b.r));
                return new Best<B>(bp,b.x,new DelayExtract<A,B>(this));
              }
              break;
            }
          }
        }

        // Otherwise, dig into as
        final Scored<A> _as = nextAs();
        if (_as instanceof EmptyOrBad)
          return (Scored)Empty$.MODULE$;
        final Best<A> as = (Best<A>)_as;
        asLazy = as.r();
        final Scored<B> _fx = f.apply(as.x());
        if (_fx instanceof Best) {
          final Best<B> fx = (Best<B>)_fx;
          if (bs == null)
            bs = new PriorityQueue<AltBase>();
          bs.add(new Biased<B>(fx.p(),fx.x(),fx.r(),as.p()));
        }
      }
    }
  }

  static final class DelayExtract<A,B> extends LazyScored<B> {
    private FlatMapState<A,B> state;
    private Scored<B> _s;

    DelayExtract(FlatMapState<A,B> state) {
      this.state = state;
    }

    public double p() {
      return _s != null ? _s.p() : state.p();
    }

    public Scored<B> s() {
      if (_s == null) {
        _s = state.extract();
        state = null;
      }
      return _s;
    }
  }
}
