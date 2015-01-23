package tarski;

import scala.Function0;
import scala.Function1;
import scala.Function2;
import scala.collection.JavaConversions;
import scala.collection.immutable.$colon$colon$;
import scala.collection.immutable.List;
import scala.collection.immutable.Nil$;
import tarski.JavaTrie.Generator;
import tarski.Scores.*;
import utility.Interrupts;

import java.util.Collection;
import java.util.PriorityQueue;

import static java.lang.Math.max;

public class JavaScores {
  // If true, failure causes are tracked via Bad.  If false, only Empty and Best are used.
  static final boolean trackErrors = false;

  // To enable probability tracking, swap the comment blocks below and make the substitution
  //   double /*Prob*/   ->   DebugProb
  // except without the space.  Also swap the definition of Prob in Scores, and fix the compile error in JavaTrie.

  // Divide two probabilities, turning infinity into 1
  static double pdiv(double x, double y) { return y == 0 ? 1 : x/y; }

  // Indirection functions so that we can swap in DebugProb for debugging
  public static final boolean trackProbabilities = false;
  static double pp(double x) { return x; }
  static double pmul(double x, double y) { return x*y; }
  static public Scores.Error ppretty(double x) { return new OneError(""+x); }
  /**/

  /*
  // Named probabilities.  Very expensive, so enable only for debugging.
  static abstract public class DebugProb {
    final double prob;
    DebugProb(double prob) { this.prob = prob; }
    public boolean equals(Object y) { return y instanceof DebugProb && prob==((DebugProb)y).prob; }
    final public String toString() { return ""+prob; }
    abstract public Scores.Error pretty();
    public boolean known() { return false; }
  }
  static final class NameProb extends DebugProb {
    final String name;
    NameProb(String name, double prob) { super(prob); this.name = name; }
    final public Scores.Error pretty() { return new OneError(prob+" : "+name); }
    @Override public boolean known() { return prob==1 && name.equals("known"); }
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
      if (x instanceof MulProb) ((MulProb)x).flatten(es); else if (!x.known()) es.add(x.pretty());
      if (y instanceof MulProb) ((MulProb)y).flatten(es); else if (!y.known()) es.add(y.pretty());
    }
  }
  static final class DivProb extends DebugProb {
    final DebugProb x,y;
    DivProb(DebugProb x, DebugProb y) { super(pdiv(x.prob,y.prob)); this.x = x; this.y = y; }
    final public Scores.Error pretty() { return nest("/ : "+prob,scalaList(x,y)); }
  }
  public static final boolean trackProbabilities = true;
  static double pp(DebugProb x) { return x.prob; }
  static DebugProb pmul(DebugProb x, DebugProb y) { return new MulProb(x,y); }
  static double pdiv(double x, DebugProb y) { return pdiv(x,y.prob); }
  static DebugProb pdiv(DebugProb x, DebugProb y) { return DivProb(x,y); }
  static public Scores.Error ppretty(DebugProb x) { return x.pretty(); }
  /**/

  // s bias q
  static final class Biased<B> extends HasProb {
    final double _p;
    final double/*Prob*/ q;
    final Scored<B> s;

    Biased(double/*Prob*/ q, Scored<B> s) {
      this.q = q;
      this.s = s;
      this._p = pp(q)*s.p();
    }

    public double p() {
      return _p;
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
      if (Interrupts.pending != 0) Interrupts.checkInterrupts();
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
              if (b.s instanceof Bad) // b.s may be empty
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
          final Best<A> ab = (Best<A>)as;
          as = ab.r();
          if (bs == null)
            bs = new PriorityQueue<Biased<B>>();
          bs.add(new Biased<B>(ab.dp(),f.apply(ab.x())));
          continue;
        } else if (bads == null)
          return (Scored)Empty$.MODULE$;
        else if (as instanceof Bad)
          bads = $colon$colon$.MODULE$.<Bad>apply((Bad)as,bads);
        return Scores.nestError("flatMap failed",bads);
      } while (p() > goal);
      // If we hit goal without finding an option, return more laziness
      return new Extractor<B>(this);
    }
  }

  // Requires p >= then.p()
  static public <A> Scored<A> uniformThen(double/*Prob*/ p, A[] xs, Scored<A> then) {
    assert pp(p) >= then.p();
    if (xs != null && xs.length>0) {
      if (trackErrors)
        then = Scores.good(then);
      for (int i=xs.length-1;i>=0;i--)
        then = new Best<A>(p,xs[i],then);
    }
    return then;
  }
  static public <A> Scored<A> uniformThen(double/*Prob*/ p, List<A> xs, Scored<A> then) {
    assert pp(p) >= then.p();
    if (xs.isEmpty())
      return then;
    if (trackErrors)
      then = Scores.good(then);
    while (xs.nonEmpty()) {
      then = new Best<A>(p,xs.head(),then);
      xs = (List)xs.tail();
    }
    return then;
  }

  // Assume no error (use Empty instead of Bad)
  static public <A> Scored<A> listGood(final List<Alt<A>> xs) {
    final int n = xs.size();
    switch (n) {
      case 0:
        return (Scored<A>)Empty$.MODULE$;
      case 1:
        final Alt<A> x = xs.head();
        return new Best<A>(x.dp(),x.x(),(Scored<A>)Empty$.MODULE$);
      default:
        final PriorityQueue<Alt<A>> pq = new PriorityQueue<Alt<A>>(JavaConversions.asJavaCollection(xs));
        final Alt<A> bestA = pq.poll();
        return new Best<A>(bestA.dp(), bestA.x(), new Extractor<A>(new MultipleAltState<A>(pq)));
    }
  }

  // Fast version of x0 ++ x1 ++ ...  The list is assumed nonempty.
  static public <A> Scored<A> multiple(List<Scored<A>> ss) {
    Scored<A> s0 = null, s1 = null;
    PriorityQueue<Scored<A>> heap = null;
    while (ss.nonEmpty()) {
      final Scored<A> s = ss.head();
      ss = (List)ss.tail();
      if (!(s instanceof Empty$)) {
        if (s0 == null)
          s0 = s;
        else if (s1 == null)
          s1 = s;
        else {
          if (heap == null) {
            heap = new PriorityQueue<Scored<A>>();
            heap.add(s0);
            heap.add(s1);
          }
          heap.add(s);
        }
      }
    }
    return s0 == null ? (Scored<A>)Empty$.MODULE$
         : s1 == null ? s0
         : heap == null ? s0.$plus$plus(s1)
         : new Extractor<A>(new MultipleState<A>(heap));
  }

  static public final class MultipleState<A> extends State<A> {
    private final PriorityQueue<Scored<A>> heap;

    // List of errors, null if we've already found at least one option.
    private List<Bad> bads;

    // The Alt's probability is an upper bound on the Scored returned by the functions
    protected MultipleState(PriorityQueue<Scored<A>> heap) {
      this.heap = heap;
      this.bads = trackErrors ? (List)Nil$.MODULE$ : null;
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
          return Scores.nestError("multiple failed",bads);
        }

        final Scored<A> s = heap.poll();
        if (s instanceof LazyScored) {
          final double limit = max(goal,p());
          heap.add(((LazyScored<A>)s).force(limit));

          // did we produce something?
          if (heap.peek() instanceof Best) {
            final Best<A> b = (Best<A>)heap.poll();
            bads=null;
            heap.add(b.r());
            return new Best<A>(b.dp(),b.x(),new Extractor<A>(this));
          }
        } else if (s instanceof Best) {
          bads = null; // We've found at least one option, so no need to track errors
          final Best<A> b = (Best<A>)s;
          heap.add(b.r());
          return new Best<A>(b.dp(),b.x(),new Extractor<A>(this));
        } else if (s instanceof Bad) {
          if (bads != null)
            bads = $colon$colon$.MODULE$.<Bad>apply((Bad)s,bads);
        }
      } while (p() > goal);
      return new Extractor<A>(this);
    }
  }

  static public final class MultipleAltState<A> extends State<A> {
    private final PriorityQueue<Alt<A>> heap;

    // The Alt's probability is an upper bound on the Scored returned by the functions
    protected MultipleAltState(final PriorityQueue<Alt<A>> heap) {
      this.heap = heap;
    }

    // Current probability bound
    public double p() {
      final Alt<A> a = heap.peek();
      return a != null ? a.p() : 0;
    }

    public Scored<A> extract(final double goal) {
      final Alt<A> s = heap.poll();
      return s == null ? (Scored<A>)Empty$.MODULE$
                       : new Best<A>(s.dp(), s.x(), heap.isEmpty() ? (Scored<A>)Empty$.MODULE$
                                                                   : new Extractor<A>(this));
    }
  }

  // For use in JavaTrie
  static public final class GeneratorState<A> extends State<A> {
    private final Generator<A> gen;
    private final PriorityQueue<Alt<String>> heap;

    // Current array we're pulling from
    private double/*Prob*/ dp;
    private A[] xs;
    private int k;

    public GeneratorState(final Generator<A> gen, final Collection<Alt<String>> input) {
      this.gen = gen;
      this.heap = new PriorityQueue<Alt<String>>(input);
    }

    public double p() {
      return xs != null ? pp(dp) : heap.peek().p();
    }

    public Scored<A> extract(final double goal) {
      do {
        if (xs == null) {
          final Alt<String> s = heap.poll();
          // s should never be null, since we check heap.isEmpty() below
          xs = gen.lookup(s.x());
          if (xs == null || xs.length==0) {
            if (heap.isEmpty())
              return (Scored<A>)Empty$.MODULE$;
            xs = null;
            continue;
          }
          dp = s.dp();
          k = 0;
        }
        final A x = xs[k++];
        if (k == xs.length)
          xs = null;
        return new Best<A>(dp,x,xs == null && heap.isEmpty() ? (Scored<A>)Empty$.MODULE$
                                                             : new Extractor<A>(this));
      } while (heap.peek().p() > goal); // We check heap.isEmpty above, so peek always succeeds here
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
      if (Interrupts.pending != 0) Interrupts.checkInterrupts();
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
    private final double _p, xp, yp;
    private Function2<A,B,C> f;
    private Scored<C> s;

    LazyProductWith(Scored<A> x, Scored<B> y, Function2<A,B,C> f) {
      this.x = x;
      this.y = y;
      this.f = f;
      xp = x.p();
      yp = y.p();
      _p = xp*yp;
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
        double xp = this.xp;
        double yp = this.yp;
        final Function2<A,B,C> f = this.f; this.f = null;
        for (boolean first=true;;first=false) {
          // Stay lazy if we're below the goal
          if (!first && xp*yp <= p) {
            s = new LazyProductWith<A,B,C>(x,y,f);
            break;
          }

          // Force either x or y if necessary.
          // If both can be forced, force the higher probability one.
          // Unfortunately, this heuristic doesn't help very much.
          final boolean xLazy = x instanceof LazyScored;
          final boolean yLazy = y instanceof LazyScored;
          if (xLazy || yLazy) {
            if (xLazy && (!yLazy || xp > yp)) {
              x = ((LazyScored<A>)x).force(pdiv(p,yp));
              xp = x.p();
              if (xp == 0 && x instanceof EmptyOrBad) {
                s = (Scored)x;
                break;
              }
            } else {
              y = ((LazyScored<B>)y).force(pdiv(p,xp));
              yp = y.p();
              if (yp == 0 && y instanceof EmptyOrBad) {
                s = (Scored)y;
                break;
              }
            }
            continue;
          }

          // We found alternatives for both x and y!
          final Best<A> _x = (Best<A>)x;
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
            final LazyScored<C> t = r0; r0 = r1; r1 = t; // Swap r0 and r1
          }
          if (r1 != null)
            r0 = new LazyPlus<C>(r0,r1);
          final Scored<C> r = r0 == null ? (Scored<C>)Empty$.MODULE$
                            : r1 == null ? r0
                                         : new LazyPlus<C>(r0,new LazyProductWith<A,B,C>(xr,yr,f));
          s = new Best<C>(pmul(xdp,ydp),f.apply(xx,yx),r);
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

    public Scored<A> force(final double q) {
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

  // Assuming x.p <= q, divide all probabilities in x by q
  static public final class LazyUnbiased<A> extends LazyScored<A> {
    private final double/*Prob*/ q;
    private final double _p;
    private LazyScored<A> x;
    private Scored<A> s;

    LazyUnbiased(final double/*Prob*/ q, final LazyScored<A> x) {
      this.q = q;
      this.x = x;
      this._p = pdiv(x.p(),q);
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(final double g) {
      if (s == null) {
        final double gq = g*pp(q);
        Scored<A> x = this.x.force(gq); this.x = null;
        for (;;) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (_x.p() > gq) {
              x = _x.force(gq);
              continue;
            } else
              s = new LazyUnbiased<A>(q,_x);
          } else if (x instanceof Best) {
            final Best<A> _x = (Best<A>)x;
            s = new Best<A>(pdiv(_x.dp(),q),_x.x(),Scores.unbiased(q,_x.r()));
          } else
            s = (Scored)x;
          break;
        }
      }
      return s;
    }
  }

  // Evaluate s enough to make sure it's nonempty, then call f(best,rest)
  static public final class LazyWhatever<A,B> extends LazyScored<B> {
    private LazyScored<A> x;
    private Function2<A,Scored<A>,B> f;
    private Scored<B> s;

    LazyWhatever(final LazyScored<A> x, final Function2<A,Scored<A>,B> f) {
      this.x = x;
      this.f = f;
    }

    public double p() {
      return x.p();
    }

    public Scored<B> force(final double q) {
      if (s == null) {
        final Function2<A,Scored<A>,B> f = this.f; this.f = null;
        Scored<A> x = this.x.force(q); this.x = null;
        for (;;) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (_x.p() > q) {
              x = _x.force(q);
              continue;
            } else
              s = new LazyWhatever<A,B>(_x,f);
          } else if (x instanceof Best) {
            final Best<A> _x = (Best<A>)x;
            final double/*Prob*/ xp = _x.dp();
            s = new Best<B>(xp,f.apply(_x.x(),Scores.unbiased(xp,_x.r())),(Scored)Empty$.MODULE$);
          } else
            s = (Scored)x;
          break;
        }
      }
      return s;
    }
  }
}
