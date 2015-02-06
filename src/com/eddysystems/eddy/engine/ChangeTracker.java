package com.eddysystems.eddy.engine;

import com.google.common.base.Predicate;
import org.jetbrains.annotations.NotNull;

import java.util.*;

// Keep track of changes that may affect the environment.
// This class is thread safe.
public class ChangeTracker<A> {
  private final Set<A> all = new HashSet<A>(); // All names that we need to know about
  private final Object recentLock = new Object();
  private @NotNull List<A> recent = new ArrayList<A>(); // Stuff not yet added to all

  public ChangeTracker() {}

  // Get the size of the change set
  public int size() {
    sync();
    synchronized (all) {
      return all.size();
    }
  }

  // Grab the current list
  public List<A> values() {
    sync();
    synchronized (all) {
      return new ArrayList<A>(all);
    }
  }

  // Forget about one value.  The caller is responsible for calling sync if appropriate.
  public void forget(final A x) {
    synchronized (all) {
      all.remove(x);
    }
  }

  // Forgot about many values
  public void forget(final A[] xs) {
    sync();
    for (final A x : xs)
      forget(x);
  }

  // Forget about all values for which pred returns true
  public void forgetIf(final Predicate<A> pred) {
    sync();
    synchronized (all) {
    Iterator<A> it = all.iterator();
      while (it.hasNext()) {
        if (pred.apply(it.next()))
          it.remove();
      }
    }
  }

  // Add a new value.  Always very fast.
  public void add(final A x) {
    synchronized (recentLock) {
      recent.add(x);
    }
  }

  // Move everything from recent to all
  public void sync() {
    final List<A> newRecent = new ArrayList<A>();
    final List<A> oldRecent;
    synchronized (recentLock) {
      oldRecent = recent;
      recent = newRecent;
    }
    synchronized (all) {
      for (final A x : oldRecent)
        all.add(x);
    }
  }
}
