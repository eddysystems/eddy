package tarski;

import scala.Function0;
import scala.Function1;
import scala.Function2;
import scala.collection.immutable.List;
import scala.collection.immutable.Nil$;
import scala.collection.immutable.$colon$colon$;
import scala.runtime.AbstractFunction1;
import tarski.Scores.*;
import static tarski.Scores.*;
import java.util.*;
import static java.lang.Math.max;

public class JavaScores {
  // s bias q
  static final class Biased<B> extends HasProb {
    final double q;
    final Scored<B> s;

    Biased(double q, Scored<B> s) {
      this.q = q;
      this.s = s;
    }

    public double p() {
      return q*s.p();
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
      if (trackErrors())
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
              continue;
            } else if (b.s instanceof Best) { // We found the best one
              bads = null; // We've found at least one thing, so no need to track errors further
              final Best<B> bb = (Best<B>)b.s;
              final Scored<B> r = bb.r();
              if (!(r instanceof Empty$))
                bs.add(new Biased<B>(b.q,r));
              return new Best<B>(b.p(),bb.x(),new Extractor<B>(this));
            } else if (bads != null) {
              bads = $colon$colon$.MODULE$.<Bad>apply((Bad)b.s,bads);
              continue;
            }
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
          bs.add(new Biased<B>(ab.p(),f.apply(ab.x())));
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
        if (more != null) {
          absorb(more.apply());
          more = null;
        }
        if (heap.isEmpty()) {
          if (error == null)
            return (Scored<A>)Empty$.MODULE$;
          return oneError(error);
        }
      }
      Alt<A> a = heap.poll();
      error = null;
      return new Best<A>(a.p(),a.x(),new Extractor<A>(this));
    }
  }

  static public final class UniformState<A> extends State<A> {
    private final double _p;
    private A[] xs;
    private int i;
    private Function0<String> error;

    public UniformState(double p, A[] xs, Function0<String> error) {
      this._p = p;
      this.xs = xs == null || xs.length == 0 ? null : xs;
      this.error = this.xs == null ? error : null;
    }

    public double p() {
      return xs == null ? 0 : _p;
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
      if (trackErrors())
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
          return new Best<A>(b.p(),b.x(),new Extractor<A>(this));
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
    private final double q;
    private final double _p;
    private Scored<A> s;

    public LazyBias(LazyScored<A> x, double q) {
      this.x = x;
      this.q = q;
      this._p = x.p()*q;
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(double p) {
      if (s == null) {
        final double pq = q==0 ? 1 : p/q;
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
            s = new Best<A>(q*_x.p(),_x.x(),_x.r().bias(q));
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
            s = new Best<A>(_x.p(),_x.x(),_x.r().$plus$plus(y));
          } else if (trackErrors() && x instanceof Bad)
            s = x.$plus$plus(y);
          else
            s = y;
          break;
        }
      }
      return s;
    }
  }

  // Lazy version of x map f
  static public final class LazyMap<A,B> extends LazyScored<B> {
    private Scored<A> x;
    private Function1<A,B> f;
    private final double _p;
    private Scored<B> s;

    LazyMap(Scored<A> x, Function1<A,B> f) {
      this.x = x;
      this.f = f;
      this._p = x.p();
    }

    public double p() {
      return _p;
    }

    public Scored<B> force(double p) {
      if (s == null) {
        final Function1<A,B> f = this.f; this.f = null;
        Scored<A> x = this.x; this.x = null;
        for (boolean first=true;;first=false) {
          if (x instanceof LazyScored) {
            final LazyScored<A> _x = (LazyScored<A>)x;
            if (first || _x.p() > p) {
              x = _x.force(p);
              continue;
            } else
              s = new LazyMap<A,B>(x,f);
          } else if (x instanceof Best) {
            final Best<A> _x = (Best<A>)x;
            s = new Best<B>(_x.p(),f.apply(_x.x()),_x.r().map(f));
          } else
            s = (Scored)x;
          break;
        }
      }
      return s;
    }
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

    static private class FX<A,B,C> extends AbstractFunction1<B,C> {
      final A x;
      final Function2<A,B,C> f;
      FX(A x, Function2<A,B,C> f) { this.x = x; this.f = f; }
      public C apply(B y) { return f.apply(x,y); }
    }

    static private class FY<A,B,C> extends AbstractFunction1<A,C> {
      final B y;
      final Function2<A,B,C> f;
      FY(B y, Function2<A,B,C> f) { this.y = y; this.f = f; }
      public C apply(A x) { return f.apply(x,y); }
    }

    public Scored<C> force(double p) {
      if (s == null) {
        final double px = yp == 0 ? 1 : p/yp;
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
            final double py = xp == 0 ? 1 : p/xp;
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
                final A xx = _x.x();
                final B yx = _y.x();
                final Scored<A> xr = _x.r();
                final Scored<B> yr = _y.r();
                final Scored<C> xr_yr = xr.productWith(yr,f),
                                xx_yr = yr.map(new FX<A,B,C>(xx,f)),
                                xr_yx = xr.map(new FY<A,B,C>(yx,f));
                s = new Best<C>(xp*_y.p(),f.apply(xx,yx),xr_yr.$plus$plus(xr_yx.$plus$plus(xx_yr)));
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
    private final double _p;
    private Function0<Scored<A>> f;
    private Scored<A> s;

    LazyBiased(double p, Function0<Scored<A>> f) {
      this._p = p;
      this.f = f;
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(double q) {
      if (s == null) {
        double pq = _p == 0 ? 1 : q/_p;
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
            s = new Best<A>(_p*_x.p(),_x.x(),_x.r().bias(_p));
          } else
            s = (Scored)x;
          break;
        }
      }
      return s;
    }
  }

  static public final class LazyBound<A> extends LazyScored<A> {
    private final double _p;
    private Function0<Scored<A>> f;
    private Scored<A> s;

    LazyBound(double p, Function0<Scored<A>> f) {
      this._p = p;
      this.f = f;
    }

    public double p() {
      return _p;
    }

    public Scored<A> force(double q) {
      if (s == null) {
        Scored<A> x = f.apply(); f = null;
        while (x instanceof LazyScored && x.p() > q)
          x = ((LazyScored<A>)x).force(q);
        s = x;
      }
      return s;
    }
  }
}
