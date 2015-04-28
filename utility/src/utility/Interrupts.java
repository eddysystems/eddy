/* Interrupts: A mechanism for injecting code into another thread.
 *
 * Java threads cannot be safely interrupted in the presence of locks,
 * so Interrupts provides a mechanism for injecting death, pauses, etc.
 * into an appropriately listening thread.  Since it is intended for use
 * in performance critical kernels, and the performance critical kernels
 * of eddy are single threaded, Interrupts is not thread safe and not
 * scalable to a large number of interruptible threads.
 *
 * To accept interrupts, the target thread to be interrupted must create an
 * Interrupter instance and register() it on that thread.  The target should
 * then periodically run
 *
 *   if (Interrupts.pending != 0) Interrupts.checkInterrupts();
 *
 * To detect and run new interrupts.  To inject an interrupt into a target thread,
 * any other thread can call interrupter.add(f) with an appropriate Runnable.
 * Currently, this mechanism is used to inject two actions into the EddyThread:
 *
 * 1. Kill orders (throw ThreadDeath)
 * 2. Pauses: release a held lock, wait for someone else to do something with that
 *    lock, grab the lock again (all inside the Runnable passed to interrupter.add).
 */

package utility;

public class Interrupts {
  // For speed, we hide the interrupt function calls behind a raw integer value.
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
        if (actions != null && actions != sentinel)
          pending--;
        actions = sentinel; // definitely no more actions on this thread.
      }
    }

    protected void finalize() throws Throwable {
      // Normally clear should be called manually, but just in case
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
