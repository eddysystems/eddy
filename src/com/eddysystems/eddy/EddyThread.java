package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyThread extends Thread {
  final Eddy eddy;
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final int lastEditLocation;
  private final Eddy.Take cont;

  // for unfettered read access
  private final @NotNull Object accessTokenLock = new Object();
  private AccessToken accessToken = null;

  private void getReadLock() {
    // wait for smart mode and get a new access token
    DumbService ds = DumbService.getInstance(project);
    while (true) {
      ds.waitForSmartMode();

      // check if we were interrupted
      if (_canceled || isInterrupted())
        throw new ThreadDeath();

      // get a new read access token
      synchronized (accessTokenLock) {
        accessToken = ApplicationManager.getApplication().acquireReadActionLock();
      }
      // did we become dumb?
      if (!ds.isDumb()) {
        _pausedForWriteAction = false;
        break;
      } else {
        // release lock and try again
        releaseReadLock();
      }
    }
    assert ApplicationManager.getApplication().isReadAccessAllowed();
  }

  private void releaseReadLock() {
    synchronized (accessTokenLock) {
      if (accessToken != null) {
        accessToken.finish();
        accessToken = null;
      }
    }
  }

  // Results, if we've found something
  Eddy.Output output = null;

  EddyThread(final Project project, final Editor editor, final int lastEditLocation, final Eddy.Take cont) {
    this.setName("Eddy thread " + getId());
    this.eddy = new Eddy(project,editor);
    this.project = project;
    this.editor = editor;
    this.lastEditLocation = lastEditLocation;
    this.cont = cont;
  }

  private boolean softInterrupts = false;
  private boolean _canceled = false;
  private boolean _pausedForWriteAction = false;
  private boolean _finished = false;

  private static final Object current_eddythread_lock = new Object();
  private static EddyThread current_eddythread = null;

  public static void run(EddyThread thread) {
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        current_eddythread.interrupt();
      }
      current_eddythread = thread;
      current_eddythread.start();
    }
  }

  // Pause the current eddy thread to wait for a write action. True if a thread was actually paused.
  public static boolean pause(Object action) {
    // The eddy thread has a read access token. Release it to let a write action start, then grab a new one.
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        current_eddythread.pauseForWriteAction();
        return true;
      }
      return false;
    }
  }

  // kills the currently active thread, if any
  public static boolean kill() {
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        current_eddythread.interrupt();
        return true;
      }
      return false;
    }
  }

  public static EddyThread getEddyThread() {
    Thread t = currentThread();
    if (t instanceof EddyThread)
      return (EddyThread)t;
    else
      return null;
  }


  public boolean canceled() {
    return _canceled;
  }

  public boolean finished() { return _finished; }

  public synchronized void setSoftInterrupts(final boolean b) {
    if (softInterrupts == b)
      return;

    softInterrupts = b;

    // if we switch to soft interrupts and we were interrupted, kill the thread now
    if (softInterrupts && isInterrupted()) {
      throw new ThreadDeath();
    }

    // if we switch back to hard interrupts and we tried interrupting before, interrupt now.
    if (!softInterrupts && _canceled) {
      interrupt();
    }
  }

  public synchronized void interrupt() {
    if (_canceled)
      return;
    _canceled = true;
    if (softInterrupts) {
      log("soft interrupting " + this.getName());
    } else {
      log("interrupting " + this.getName());
      super.interrupt();
    }
  }

  public synchronized void pauseForWriteAction() {
    if (_canceled || _pausedForWriteAction)
      return;
    _pausedForWriteAction = true;
  }

  public Runnable getThisThreadChecker() {
    return new Runnable() {
      @Override public void run() {
        // check if we should pause to allow a write action
        if (!_pausedForWriteAction) {
          // check if we were interrupted
          if (_canceled || isInterrupted())
            throw new ThreadDeath();
        } else {
          // relinquish control of read access token
          releaseReadLock();
          // let other threads work it out (this ought to run the write action if we're responsible for the holdup)
          try {
            sleep(0);
          } catch (InterruptedException e) {
            throw new ThreadDeath();
          }

          getReadLock();
          _pausedForWriteAction = false;
        }
      }
    };
  }

  static public Runnable getThreadChecker() {
    return ((EddyThread)Thread.currentThread()).getThisThreadChecker();
  }

  @Override
  public void run() {
    try {
      getReadLock();
      EddyPlugin.getInstance(project).getWidget().moreBusy();
      try {
        eddy.process(editor, lastEditLocation, new Eddy.Take() {
          @Override public boolean take(Eddy.Output output) {
            EddyThread.this.output = output;
            return cont.take(output);
          }
        });
        _finished = true;
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
      releaseReadLock();
    }
  }
}
