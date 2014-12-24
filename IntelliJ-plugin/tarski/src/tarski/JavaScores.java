package tarski;

import scala.Function0;
import scala.Function1;
import scala.Function2;
import scala.collection.immutable.$colon$colon$;
import scala.collection.immutable.List;
import scala.collection.immutable.Nil$;
import tarski.Scores.*;

import java.util.ArrayList;
import java.util.PriorityQueue;
import static java.lang.Math.max;
import static tarski.Scores.oneError;
import static tarski.Scores.nestError;

public class JavaScores {
  // If true, failure causes are tracked via Bad.  If false, only Empty and Best are used.
  static final boolean trackErrors = false;

  // To enable probability tracking, swap the comment blocks below and make the substitution
  //   double /*Prob*/   ->   DebugProb
  // except without the space.  Also swap the definition of Prob in Scores, and fix the compile error in JavaTrie.

  // Divide two probabilities, turning infinity into 2
  static double pdiv(double x, double y) { return y == 0 ? 2 : x/y; }

  // Indirection functions so that we can swap in DebugProb for debugging
  static final boolean trackProbabilities = false;
  static double pp(double x) { return x; }
  static double pmul(double x, double y) { return x*y; }
  static final double pzero = 0;
  static double padd(double x, double y) { return x+y; }
  static double pcomp(double x) { return 1-x; }
  static public Scores.Error ppretty(double x) { return new OneError(""+x); }
  /**/

  // Named probabilities.  Very expensive, so enable only for debugging.
  /*
  static abstract public class DebugProb {
    final double prob;
    DebugProb(double prob) { this.prob = prob; }
    public boolean equals(Object y) { return y instanceof DebugProb && prob==((DebugProb)y).prob; }
    final public String toString() { return ""+prob; }
    abstract public Scores.Error pretty();
  }
  static final class NameProb extends DebugProb {
    final String name;
    NameProb(String name, double prob) { super(prob); this.name = name; }
    final public Scores.Error pretty() { return new OneError(prob+" : "+name); }
  }
  static private NestError nest(String e, Scores.Error... es) {
    List xs = (List)Nil$.MODULE$;
    for (int i=es.length-1;i>=0;i--) xs = $colon$colon$.MODULE$.apply(es[i],xs);
    return new NestError(e,xs);
  }
  static final class MulProb extends DebugProb {
    final DebugProb x,y;
    MulProb(DebugProb x, DebugProb y) { super(x.prob*y.prob); this.x = x; this.y = y; }
    final public Scores.Error pretty() {
      final ArrayList<Scores.Error> es = new ArrayList<Scores.Error>();
      flatten(es);
      return nest("* : "+prob,es.toArray(new Scores.Error[es.size()]));
    }
    private void flatten(ArrayList<Scores.Error> es) {
      if (x instanceof MulProb) ((MulProb)x).flatten(es); else es.add(x.pretty());
      if (y instanceof MulProb) ((MulProb)y).flatten(es); else es.add(y.pretty());
    }
  }
  static final class AddProb extends DebugProb {
    final DebugProb x,y;
    AddProb(DebugProb x, DebugProb y) { super(x.prob+y.prob); this.x = x; this.y = y; }
    final public Scores.Error pretty() {
      final ArrayList<Scores.Error> es = new ArrayList<Scores.Error>();
      flatten(es);
      return nest("+ : "+prob,es.toArray(new Scores.Error[es.size()]));
    }
    private void flatten(ArrayList<Scores.Error> es) {
      if (x instanceof AddProb) ((AddProb)x).flatten(es); else es.add(x.pretty());
      if (y instanceof AddProb) ((AddProb)y).flatten(es); else es.add(y.pretty());
    }
  }
  static final class CompProb extends DebugProb {
    final DebugProb x;
    CompProb(DebugProb x) { super(1-x.prob); this.x = x; }
    final public Scores.Error pretty() {
      final Scores.Error e = x.pretty();
      return e instanceof OneError ? new OneError("1 - "+((OneError)e).e()) : nest("1 -",e);
    }
  }
  static final boolean trackProbabilities = true;
  static double pp(DebugProb x) { return x.prob; }
  static DebugProb pmul(DebugProb x, DebugProb y) { return new MulProb(x,y); }
  static final DebugProb pzero = null;
  static DebugProb padd(DebugProb x, DebugProb y) { return y==pzero ? x : new AddProb(x,y); }
  static DebugProb pcomp(DebugProb x) { return new CompProb(x); }
  static double pdiv(double x, DebugProb y) { return pdiv(x,y.prob); }
  static public Scores.Error ppretty(DebugProb x) { return x.pretty(); }
  /**/

  // s bias q
  static final class Biased<B> extends HasProb {
    final double/*Prob*/ q;
    final Scored<B> s;

    Biased(double/*Prob*/ q, Scored<B> s) {
      this.q = q;
      this.s = s;
    }

    public double p() {
      return pp(q)*s.p();
    }
  }

  static abstract public class State<A> {
    // Current probability bound.  May decrease over time.
    abstract public double p();

    // Once called, the extractor should be discarded.
    abstract public Scored<A> extract(double p);
  }

  static public final class Extractor<A> extends LazyScored<A> {
    private final double _p;
    private State<A> state;
    private Scored<A> _s;

    public Extractor(State<A> state) {
      this._p = state.p();
      this.state = state;
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(double p) {
      if (_s == null) {
        _s = state.extract(p);
        state = null;
      }
      return _s;
    }
  }

  static public final class FlatMapState<A,B> extends State<B> {
    private final Function1<A,Scored<B>> f; // Our flatMap function
    private Scored<A> as; // Unprocessed input
    private PriorityQueue<Biased<B>> bs; // Sorted processed output
    private List<Bad> bads; // List of errors, null if we've already found something

    public FlatMapState(Scored<A> input, Function1<A,Scored<B>> f) {
      this.as = input;
      this.f = f;
      if (trackErrors)
        bads = (List)Nil$.MODULE$;
    }

    public double p() {
      return max(as.p(), bs == null || bs.isEmpty() ? 0 : bs.peek().p());
    }

    public Scored<B> extract(final double goal) {
      do {
        // If bs is better than as, we may be done
        if (bs != null) {
          final double asp = as.p();
          final Biased<B> b = bs.peek();
          if (b != null && b.p() >= asp) {
            bs.poll();
            if (b.s instanceof LazyScored) { // Force and add back to heap
              final double limit = max(max(goal,asp),bs.isEmpty() ? 0 : bs.peek().p());
              bs.add(new Biased<B>(b.q,((LazyScored<B>)b.s).force(limit)));
            } else if (b.s instanceof Best) { // We found the best one
              bads = null; // We've found at least one thing, so no need to track errors further
              final Best<B> bb = (Best<B>)b.s;
              final Scored<B> r = bb.r();
              if (!(r instanceof Empty$))
                bs.add(new Biased<B>(b.q,r));
              return new Best<B>(pmul(b.q,bb.dp()),bb.x(),new Extractor<B>(this));
            } else if (bads != null)
              bads = $colon$colon$.MODULE$.<Bad>apply((Bad)b.s,bads);
            continue;
          }
        }
        // Otherwise, dig into as
        if (as instanceof LazyScored) {
          final double limit = max(goal,bs==null || bs.isEmpty() ? 0 : bs.peek().p());
          as = ((LazyScored<A>)as).force(limit);
          continue;
        } else if (as instanceof Best) {
          final Best<A> ab = (Best<A>)this.as;
          as = ab.r();
          if (bs == null)
            bs = new PriorityQueue<Biased<B>>();
          bs.add(new Biased<B>(ab.dp(),f.apply(ab.x())));
          continue;
        } else if (bads == null)
          return (Scored)Empty$.MODULE$;
        else if (as instanceof Bad)
          bads = $colon$colon$.MODULE$.<Bad>apply((Bad)as,bads);
        return nestError("flatMap failed",bads);
      } while (p() > goal);
      // If we hit goal without finding an option, return more laziness
      return new Extractor<B>(this);
    }
  }

  static public final class OrderedAlternativeState<A> extends State<A> {
    private PriorityQueue<Alt<A>> heap;
    private Function0<List<Alt<A>>> more;
    private Function0<String> error;

    // Requires: prob first >= prob andThen
    public OrderedAlternativeState(List<Alt<A>> list, Function0<List<Alt<A>>> more, Function0<String> error) {
      this.error = error;
      heap = new PriorityQueue<Alt<A>>();
      absorb(list);
      if (more != null && heap.isEmpty())
        absorb(more.apply());
      else
        this.more = more;
    }

    private void absorb(List<Alt<A>> list) {
      while (!list.isEmpty()) {
        heap.add(list.head());
        list = (List<Alt<A>>)list.tail();
      }
    }

    // Current probability bound
    public double p() {
      Alt<A> a = heap.peek();
      return a == null ? 0 : a.p();
    }

    public Scored<A> extract(final double goal) {
      if (heap.isEmpty()) {
        if (error == null)
          return (Scored<A>)Empty$.MODULE$;
        return oneError(error);
      }
      Alt<A> a = heap.poll();
      error = null;
      if (heap.isEmpty() && more != null) {
        absorb(more.apply());
        more = null;
      }
      return new Best<A>(a.dp(),a.x(),new Extractor<A>(this));
    }
  }

  static public final class UniformState<A> extends State<A> {
    private final double/*Prob*/ _p;
    private A[] xs;
    private int i;
    private Function0<String> error;

    public UniformState(double/*Prob*/ p, A[] xs, Function0<String> error) {
      this._p = p;
      this.xs = xs == null || xs.length == 0 ? null : xs;
      this.error = this.xs == null ? error : null;
    }

    public double p() {
      return xs == null ? 0 : pp(_p);
    }

    public Scored<A> extract(final double goal) {
      if (xs == null) {
        if (error == null)
          return (Scored<A>)Empty$.MODULE$;
        return oneError(error);
      }
      final A x = xs[i++];
      if (i == xs.length)
        xs = null;
      return new Best<A>(_p,x,new Extractor<A>(this));
    }
  }

  static public final class MultipleState<A> extends State<A> {
    private final PriorityQueue<Scored<A>> heap;

    // List of errors, null if we've already found at least one option.
    private List<Bad> bads;

    // The Alt's probability is an upper bound on the Scored returned by the functions
    public MultipleState(List<Scored<A>> options) {
      assert options.nonEmpty();
      heap = new PriorityQueue<Scored<A>>();
      while (options.nonEmpty()) {
        heap.add(options.head());
        options = (List)options.tail();
      }
      if (trackErrors)
        bads = (List)Nil$.MODULE$;
    }

    // Current probability bound
    public double p() {
      final Scored<A> a = heap.peek();
      return a == null ? 0 : a.p();
    }

    public Scored<A> extract(final double goal) {
      do {
        if (heap.isEmpty()) {
          if (bads == null)
            return (Scored<A>)Empty$.MODULE$;
          return nestError("multiple failed",bads);
        }
        final Scored<A> s = heap.poll();
        if (s instanceof LazyScored) {
          final double limit = max(goal,p());
          heap.add(((LazyScored<A>)s).force(limit));
        } else if (s instanceof Best) {
          bads = null; // We've found at least one option, so no need to track errors
          final Best<A> b = (Best<A>)s;
          heap.add(b.r());
          return new Best<A>(b.dp(),b.x(),new Extractor<A>(this));
        } else if (bads != null)
          bads = $colon$colon$.MODULE$.<Bad>apply((Bad)s,bads);
      } while (heap.isEmpty() || heap.peek().p() > goal);
      // If we hit goal without finding an option, return more laziness
      return new Extractor<A>(this);
    }
  }

  // Lazy version of x bias q
  static public final class LazyBias<A> extends LazyScored<A> {
    private LazyScored<A> x;
    private final double/*Prob*/ q;
    private final double _p;
    private Scored<A> s;

    public LazyBias(LazyScored<A> x, double/*Prob*/ q) {
      this.x = x;
      this.q = q;
      this._p = x.p()*pp(q);
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(double p) {
      if (s == null) {
        final double pq = pdiv(p,q);
        Scored<A> x = this.x.force(pq); this.x = null;
        for (;;) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (_x.p() > pq) {
              x = _x.force(pq);
              continue;
            } else
              s = new LazyBias<A>(_x,q);
          } else if (x instanceof Best) {
            final Best<A> _x = (Best<A>)x;
            s = new Best<A>(pmul(q,_x.dp()),_x.x(),_x.r()._bias(q));
          } else
            s = x;
          break;
        }
      }
      return s;
    }
  }

  // Lazy version of x ++ y, assuming x.p >= y.p
  static public final class LazyPlus<A> extends LazyScored<A> {
    private LazyScored<A> x;
    private Scored<A> y;
    private final double _p;
    private Scored<A> s;

    LazyPlus(LazyScored<A> x, Scored<A> y) {
      this.x = x;
      this.y = y;
      this._p = x.p();
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(double p) {
      if (s == null) {
        Scored<A> x = this.x.force(max(p,y.p())); this.x = null;
        Scored<A> y = this.y; this.y = null;
        for (;;) {
          if (x.p() < y.p()) {
            Scored<A> t = x; x = y; y = t;
          }
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (x.p() > p) {
              x = _x.force(max(p,y.p()));
              continue;
            } else
              s = new LazyPlus<A>(_x,y);
          } else if (x instanceof Best) {
            final Best<A> _x = (Best<A>)x;
            s = new Best<A>(_x.dp(),_x.x(),_x.r().$plus$plus(y));
          } else if (trackErrors && x instanceof Bad)
            s = x.$plus$plus(y);
          else
            s = y;
          break;
        }
      }
      return s;
    }
  }

  // Lazy version of x map f bias p, where f and p are abstract.
  static public abstract class LazyMapBase<A,B> extends LazyScored<B> {
    private Scored<A> x;
    private final double _p;
    private Scored<B> s;

    LazyMapBase(Scored<A> x, double bound) {
      this.x = x;
      this._p = bound;
    }

    // Abstract interface
    abstract protected LazyMapBase<A,B> clone(Scored<A> x); // Map a different Scored
    abstract protected Best<B> map(double/*Prob*/ p, A x, Scored<B> r); // Apply the map

    public double p() {
      return _p;
    }

    public Scored<B> force(double p) {
      if (s == null) {
        Scored<A> x = this.x; this.x = null;
        for (boolean first=true;;first=false) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (first || _x.p() > p) {
              x = _x.force(p);
              if (x instanceof EmptyOrBad)
                s = (Scored<B>)x;
              else
                continue;
            } else
              s = clone(x);
          } else { // LazyMapBase is used only for LazyScored or Best
            final Best<A> _x = (Best<A>)x;
            final Scored<A> xr = _x.r();
            final Scored<B> fr = xr instanceof EmptyOrBad ? (Scored<B>)Empty$.MODULE$ : clone(xr);
            s = map(_x.dp(),_x.x(),fr);
          }
          break;
        }
      }
      return s;
    }
  }

  // Lazy version of x map f
  static public final class LazyMap<A,B> extends LazyMapBase<A,B> {
    private Function1<A,B> f;
    LazyMap(Scored<A> x, Function1<A,B> f) { super(x,x.p()); this.f = f; }
    protected LazyMap<A,B> clone(Scored<A> x) { return new LazyMap<A,B>(x,f); }
    protected Best<B> map(double/*Prob*/ p, A x, Scored<B> r) { return new Best<B>(p,f.apply(x),r); }
  }

  // Lazy version of x.productWith(y)(f)
  static public final class LazyProductWith<A,B,C> extends LazyScored<C> {
    private Scored<A> x;
    private Scored<B> y;
    private final double _p, yp;
    private Function2<A,B,C> f;
    private Scored<C> s;

    LazyProductWith(Scored<A> x, Scored<B> y, Function2<A,B,C> f) {
      this.x = x;
      this.y = y;
      this.f = f;
      yp = y.p();
      _p = x.p()*yp;
    }

    public double p() {
      return _p;
    }

    static private final class FX<A,B,C> extends LazyMapBase<B,C> {
      private final double/*Prob*/ px;
      private final A x;
      private final Function2<A,B,C> f;
      FX(double/*Prob*/ px, A x, Scored<B> y, Function2<A,B,C> f) { super(y,pp(px)*y.p()); this.px = px; this.x = x; this.f = f; }
      protected FX<A,B,C> clone(Scored<B> y) { return new FX<A,B,C>(px,x,y,f); }
      protected Best<C> map(double/*Prob*/ py, B y, Scored<C> r) { return new Best<C>(pmul(px,py),f.apply(x,y),r); }
    }

    static private final class FY<A,B,C> extends LazyMapBase<A,C> {
      private final double/*Prob*/ py;
      private final B y;
      private final Function2<A,B,C> f;
      FY(double/*Prob*/ py, Scored<A> x, B y, Function2<A,B,C> f) { super(x,x.p()*pp(py)); this.py = py; this.y = y; this.f = f; }
      protected FY<A,B,C> clone(Scored<A> x) { return new FY<A,B,C>(py,x,y,f); }
      protected Best<C> map(double/*Prob*/ px, A x, Scored<C> r) { return new Best<C>(pmul(px,py),f.apply(x,y),r); }
    }

    public Scored<C> force(double p) {
      if (s == null) {
        final double px = pdiv(p,yp);
        Scored<A> x = this.x; this.x = null;
        Scored<B> y = this.y; this.y = null;
        final Function2<A,B,C> f = this.f; this.f = null;
        for (boolean first=true;;first=false) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (first || _x.p() > px) {
              x = _x.force(px);
              continue;
            } else
              s = new LazyProductWith<A,B,C>(x,y,f);
          } else if (x instanceof EmptyOrBad)
            s = (Scored)x;
          else {
            final Best<A> _x = (Best<A>)x;
            final double xp = _x.p();
            final double py = pdiv(p,xp);
            for (;;first=false) {
              if (y instanceof LazyScored) {
                final LazyScored<B> _y = (LazyScored<B>)y;
                if (first || y.p() > py) {
                  y = _y.force(py);
                  continue;
                } else
                  s = new LazyProductWith<A,B,C>(x,y,f);
              } else if (y instanceof EmptyOrBad)
                s = (Scored)y;
              else {
                final Best<B> _y = (Best<B>)y;
                final double/*Prob*/ xdp = _x.dp();
                final double/*Prob*/ ydp = _y.dp();
                final A xx = _x.x();
                final B yx = _y.x();
                final Scored<A> xr = _x.r();
                final Scored<B> yr = _y.r();
                LazyScored<C> r0 = yr instanceof EmptyOrBad ? null : new FX<A,B,C>(xdp,xx,yr,f),
                              r1 = xr instanceof EmptyOrBad ? null : new FY<A,B,C>(ydp,xr,yx,f);
                if ((r0 == null ? -1 : r0.p()) < (r1 == null ? -1 : r1.p())) {
                  final LazyScored<C> t = r0; r0 = r1; r1 = t;
                }
                if (r1 != null)
                  r0 = new LazyPlus<C>(r0,r1);
                final Scored<C> r = r0 == null ? (Scored<C>)Empty$.MODULE$
                                               : new LazyPlus<C>(r0,new LazyProductWith<A,B,C>(xr,yr,f));
                s = new Best<C>(pmul(xdp,ydp),f.apply(xx,yx),r);
              }
              break;
            }
            break;
          }
          break;
        }
      }
      return s;
    }
  }

  static public final class LazyBiased<A> extends LazyScored<A> {
    private final double/*Prob*/ _p;
    private Function0<Scored<A>> f;
    private Scored<A> s;

    LazyBiased(double/*Prob*/ p, Function0<Scored<A>> f) {
      this._p = p;
      this.f = f;
    }

    public double p() {
      return pp(_p);
    }

    public Scored<A> force(double q) {
      if (s == null) {
        final double pq = pdiv(q,_p);
        Scored<A> x = f.apply(); f = null;
        for (;;) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (_x.p() > pq) {
              x = _x.force(pq);
              continue;
            } else
              s = new LazyBias<A>(_x,_p);
          } else if (x instanceof Best) {
            final Best<A> _x = (Best<A>)x;
            s = new Best<A>(pmul(_p,_x.dp()),_x.x(),_x.r()._bias(_p));
          } else
            s = (Scored)x;
          break;
        }
      }
      return s;
    }
  }
}
