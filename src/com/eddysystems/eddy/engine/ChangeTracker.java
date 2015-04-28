/* ChangeTracker: Keep track of changes that may affect the environment
 *
 * Precomputation of eddy's environment data structures is fairly slow, so we can
 * only afford to do it occasionally.  In between, we maintain an incremental list
 * of changes that happened since the last environment scan.
 *
 * This class is thread safe.
 */

package com.eddysystems.eddy.engine;

import gnu.trove.TObjectLongHashMap;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ChangeTracker<A> {
  private long time = 1; // Current "time" for snapshot and forget purposes
  private final TObjectLongHashMap<A> all = new TObjectLongHashMap<A>(); // Tracked names and their last update time
  private final Object recentLock = new Object();
  private @NotNull List<A> recent = new ArrayList<A>(); // Stuff not yet added to all

  public ChangeTracker() {}

  // Grab the current list
  synchronized public List<A> values() {
    sync();
    return (List)Arrays.asList(all.keys());
  }

  // Take a snapshot of the current set, which can later be forgotten
  synchronized public Snapshot snapshot() {
    sync();
    return new Snapshot(this,time++);
  }

  // Forget about everything up to the given snapshot
  synchronized public void forget(final Snapshot snap) {
    assert snap.self == this;
    sync();
    for (final Object xo : all.keys()) {
      final A x = (A)xo;
      if (all.get(x) <= snap.time)
        all.remove(x);
    }
  }

  // Add a new value.  Always very fast.
  public void add(final @NotNull A x) {
    synchronized (recentLock) {
      recent.add(x);
    }
  }

  /****** Private stuff below ******/

  // Move everything from recent to all
  synchronized private void sync() {
    final List<A> newRecent = new ArrayList<A>();
    final List<A> oldRecent;
    synchronized (recentLock) {
      oldRecent = recent;
      recent = newRecent;
    }
    for (final A x : oldRecent)
      all.put(x,time);
  }

  // Opaque snapshot class
  public static final class Snapshot {
    private final ChangeTracker self;
    private final long time;
    private Snapshot(final ChangeTracker self, final long time) { this.self = self; this.time = time; }
  }
}
