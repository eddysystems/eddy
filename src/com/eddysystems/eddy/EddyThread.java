package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyThread extends Thread {
  final Eddy eddy;
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final int lastEditLocation;
  private final Eddy.Take cont;

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

  @Override
  public void run() {
    try {
      // must be in smart mode, must be inside read action
      final long start = System.nanoTime();
      DumbService.getInstance(project).runReadActionInSmartMode(new Runnable() {
        @Override
        public void run() {
          try {
            EddyPlugin.getInstance(project).getWidget().moreBusy();

            eddy.process(editor, lastEditLocation, new Eddy.Take() {
              @Override public boolean take(Eddy.Output output) {
                EddyThread.this.output = output;
                return cont.take(output);
              }
            });
          } finally {
            if (EddyPlugin.getInstance(project) != null && EddyPlugin.getInstance(project).getWidget() != null)
              EddyPlugin.getInstance(project).getWidget().lessBusy();
          }
        }
      });
    } catch (RuntimeInterruptedException e) {
      // interrupted while sleeping inside DumbService, ignore
    }
  }
}
