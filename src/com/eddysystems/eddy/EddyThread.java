package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import utility.Interrupts;

import java.util.EmptyStackException;
import java.util.Stack;

import static com.eddysystems.eddy.engine.Utility.log;

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
  private boolean softInterrupts = false;
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

  // the thread maintains a list of these, each one represents someone who is waiting for the thread to release its read lock.
  // Whoever is waiting will call release when it's done with whatever it needed to do before we are allowed to
  static class Pause {
    private boolean _active = false;
    synchronized void release() {
      _active = false;
      this.notifyAll();
    }
    // put this thread to sleep until release() is called
    synchronized public void finish() throws InterruptedException {
      while (_active)
        this.wait();
    }
  }
  final private Stack<Pause> waiting = new Stack<Pause>();

  // Pause the current eddy thread to wait for a write action. True if the thread was paused
  public static Pause pause() {
    // The eddy thread has a read access token. Release it to let a write action start, then grab a new one.
    final EddyThread thread = currentThread;
    if (thread == null)
      return null;

    synchronized (thread) {
      // make a new pause object and make sure it's registered
      Pause w = new Pause();
      boolean doRegister;
      synchronized (thread.waiting) {
        doRegister = thread.waiting.empty();
        thread.waiting.push(w);
      }

      if (doRegister)
        thread.interrupter.add(new Runnable() {
          public void run() {
            // relinquish control of read access token
            thread.readLock.unlock();
            // cede control until all Pause objects have been released
            try {
              final Pause pause;
              synchronized (thread.waiting) {
                pause = thread.waiting.pop();
              }
              pause.finish();
            } catch (EmptyStackException e) {
              // we're done.
            } catch (InterruptedException e) {
              throw new ThreadDeath();
            }
            thread.readLock.lock();
          }
        });

      return w;
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

  public synchronized void setSoftInterrupts(final boolean on) {
    if (softInterrupts == on)
      return;
    softInterrupts = on;

    // if we switch to soft interrupts and we were interrupted, kill the thread now
    if (on && isInterrupted())
      throw new ThreadDeath();

    // if we switch back to hard interrupts and we tried interrupting before, interrupt now.
    if (!on && _canceled)
      hardInterrupt();
  }

  public synchronized void interrupt() {
    if (_canceled)
      return;
    _canceled = true;
    if (softInterrupts)
      log("soft interrupting " + this.getName());
    else
      hardInterrupt();
  }

  private void hardInterrupt() {
    log("interrupting " + this.getName());
    interrupter.add(new Runnable() { public void run() {
      throw new ThreadDeath();
    }});
    super.interrupt();
  }

  @Override
  public void run() {
    try {
      interrupter.register();
      readLock.lock();
      EddyPlugin.getInstance(project).getWidget().moreBusy();
      try {
        eddy.process(editor, lastEditLocation, new Eddy.Take() {
          @Override public boolean take(Eddy.Output output) {
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
    } catch (RuntimeInterruptedException e) {
      // interrupted while sleeping or waiting inside getReadLock, ignore
    } catch (ThreadDeath e) {
      // interrupted while sleeping or waiting inside getReadLock, ignore
    } finally {
      readLock.unlock();
      interrupter.clear();
    }
  }
}
