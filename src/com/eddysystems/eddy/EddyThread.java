package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import utility.Interrupts;

public class EddyThread extends Thread {
  // Current thread
  private static final Object currentLock = new Object();
  private static EddyThread currentThread = null;

  // Information about our computation
  private final Eddy eddy;
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final int lastEditLocation;
  private final Eddy.Take cont;
  private final Interrupts.Interrupter interrupter;
  private final SmartReadLock readLock;

  // Results, if we've found something
  Eddy.Output output = null;

  // Cancelation flags
  private int softInterrupts = 0;
  private boolean _canceled = false;

  EddyThread(final @NotNull Project project, final @NotNull Editor editor, final int lastEditLocation, final Eddy.Take cont) {
    this.setName("Eddy thread " + getId());
    this.eddy = new Eddy(project,editor);
    this.project = project;
    this.editor = editor;
    this.lastEditLocation = lastEditLocation;
    this.cont = cont;
    this.interrupter = new Interrupts.Interrupter();
    this.readLock = new SmartReadLock(project);
  }

  public static void run(final EddyThread thread) {
    // We might be grabbing the lock for a long time, so do it on a pooled thread.
    ApplicationManager.getApplication().executeOnPooledThread(new Runnable() { public void run() {
      synchronized (currentLock) {
        final EddyThread previous = currentThread;
        if (previous != null) {
          previous.interrupt();
          try {
            previous.join();
          } catch (final InterruptedException e) {
            return;
          }
        }
        currentThread = thread;
        thread.start();
      }
    }});
  }

  // Number of active pauses, and a lock to wait on
  private static int pauses = 0;
  private static final Object pauseLock = new Object();

  // Pause the current eddy thread to wait for a write action
  public static void pause() {
    // Increment the pauses counter and register a pause action if we're the first
    final boolean register;
    synchronized (pauseLock) {
      register = pauses == 0;
      pauses++;
    }
    if (register) {
      final EddyThread thread = currentThread;
      if (thread != null)
        thread.interrupter.add(new Runnable() { public void run() {
          final boolean locked = thread.readLock.unlock();
          pauseWait();
          if (locked)
            thread.readLock.lock();
        }});
    }
  }

  // Cede control until all pauses complete
  private static void pauseWait() {
    assert !ApplicationManager.getApplication().isReadAccessAllowed();
    try {
      synchronized (pauseLock) {
        while (pauses > 0)
          pauseLock.wait();
      }
    } catch (InterruptedException e) {
      throw new ThreadDeath();
    }
  }

  // Must be called *at least* once for each call to pause()
  public static void unpause() {
    synchronized (pauseLock) {
      // Decrement the pauses counter and notify if we hit zero.
      assert pauses > 0;
      pauses--;
      if (pauses == 0)
        pauseLock.notify();
    }
  }

  // kills the currently active thread, if any
  public static boolean kill() {
    final EddyThread thread = currentThread;
    if (thread == null)
      return false;
    thread.interrupt();
    return true;
  }

  public static EddyThread getEddyThread() {
    final Thread t = currentThread();
    return t instanceof EddyThread ? (EddyThread)t : null;
  }

  public boolean canceled() {
    return _canceled;
  }

  public synchronized void pushSoftInterrupts() {
    // If we switch to soft interrupts and we were interrupted, kill the thread now
    if (softInterrupts==0 && isInterrupted())
      throw new ThreadDeath();

    softInterrupts++;
  }

  // Always call from a finally block
  public synchronized void popSoftInterrupts() {
    assert softInterrupts > 0;
    softInterrupts--;

    // If we switch back to hard interrupts and we tried interrupting before, interrupt now.
    if (softInterrupts==0 && _canceled) {
      //log("interrupting " + this.getName());
      super.interrupt();
      throw new ThreadDeath();
    }
  }

  public synchronized void interrupt() {
    if (_canceled)
      return;
    _canceled = true;
    if (softInterrupts > 0) {
      //log("soft interrupting " + this.getName());
    } else {
      //log("interrupting " + this.getName());
      interrupter.add(new Runnable() { public void run() {
        throw new ThreadDeath();
      }});
      super.interrupt();
    }
  }

  @Override
  public void run() {
    interrupter.register();
    try {
      pauseWait();
      readLock.lock();
      try {
        EddyPlugin.getInstance(project).getWidget().moreBusy();
        try {
          eddy.process(lastEditLocation, new Eddy.Take() {
            @Override public double take(final Eddy.Output output) {
              EddyThread.this.output = output;
              return cont.take(output);
            }
          });
        } catch (IndexNotReadyException e) {
          // a dumb mode started while we were running our thread, and we were not fast enough to catch it. This thread is dead. So be it.
        } finally {
          if (EddyPlugin.getInstance(project) != null && EddyPlugin.getInstance(project).getWidget() != null)
            EddyPlugin.getInstance(project).getWidget().lessBusy();
        }
      } finally {
        readLock.unlock();
      }
    } catch (RuntimeInterruptedException e) {
      // interrupted while sleeping or waiting inside getReadLock, ignore
    } catch (ThreadDeath e) {
      // interrupted while sleeping or waiting inside getReadLock, ignore
    } finally {
      interrupter.clear();
    }
  }
}
