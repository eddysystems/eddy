package utility;

import java.util.List;

public class Interrupts {
  // For speed, we hide the interrupt function calls behind a raw boolean value.
  // To check for interrupts, a thread should do
  //   if (Interrupts.pending != 0) Interrupts.checkInterrupts();

  // How many actions are pending.  Modifications are synchronized through interrupters
  public static int pending = 0;

  // Actually check for interrupts, running any actions that others have registered.
  public static void checkInterrupts() {
    final Interrupter I = interrupters.get();
    if (I != null && I.actions != null) { // Check first outside the lock
      final Runnable act;
      synchronized (interrupters) { // Check again inside the lock
        act = I.actions;
        if (act != null && act != sentinel) {
          I.actions = null;
          pending--;
        }
      }
      if (act != null && act != sentinel)
        act.run();
    }
  }

  // One interrupter per thread
  private static final ThreadLocal<Interrupter> interrupters = new ThreadLocal<Interrupter>();

  // Sentinel runnable for use with clear
  private static final Runnable sentinel = new Runnable() { public void run() { assert false; } };

  // Interrupt handle for a thread.
  // WARNING: If register and clean aren't used in the correct way, bad things will happen.
  public static final class Interrupter {
    protected Runnable actions = null;

    // Register this interrupter with the *current* thread.
    // Dies if an interrupter has already been registered.
    // Typically, this is called at the top of Thread.run().
    public void register() {
      assert interrupters.get() == null;
      interrupters.set(this);
    }

    // Clean up any actions that haven't fired.
    // WARNING: If this is not called, pending will stay positive and other threads using Interrupts will get slower.
    // Once clear is called, further actions will be ignored.
    public void clear() {
      synchronized (interrupters) {
        if (actions != null && actions != sentinel) {
          actions = sentinel;
          pending--;
        }
      }
    }
    // Normally clear should be called manually, but just in case
    protected void finalize() throws Throwable {
      clear();
      super.finalize();
    }

    // Add an action to run in the thread in which this Interrupter was registered.
    public void add(final Runnable next) {
      synchronized (interrupters) {
        if (actions == null) {
          actions = next;
          pending++;
        } else if (actions != sentinel) {
          final Runnable prev = actions;
          actions = new Runnable() { public void run() { next.run(); prev.run(); } };
        }
      }
    }
  }
}
