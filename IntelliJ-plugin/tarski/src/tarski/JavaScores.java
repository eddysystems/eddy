package tarski;

import scala.Function0;
import scala.Function1;
import scala.collection.immutable.List;
import scala.collection.immutable.Nil$;
import scala.collection.immutable.$colon$colon$;
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
}
