package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.ui.LightweightHint;
import com.intellij.util.Consumer;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyThread extends Thread {
  final Eddy eddy;
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final int lastEditLocation;
  private final Consumer<Eddy> cont;

  EddyThread(final Project project, final Editor editor, final int lastEditLocation, final Consumer<Eddy> cont) {
    this.setName("Eddy thread " + getId());
    this.eddy = new Eddy(project);
    this.project = project;
    this.editor = editor;
    this.lastEditLocation = lastEditLocation;
    this.cont = cont;
  }

  private boolean softInterrupts = false;
  private boolean _canceled = false;

  public boolean canceled() {
    return _canceled;
  }

  public synchronized void setSoftInterrupts(boolean b) {
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
    eddy.cancel();
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
          // if we waited for smart mode, don't wait too much longer
          try {
            int millis = new Double(200-(System.nanoTime()-start)/1e3).intValue();
            if (millis > 0)
              sleep(millis);

            try {
              EddyPlugin.getInstance(project).getWidget().moreBusy();

              eddy.process(editor,lastEditLocation,null);
              if (isInterrupted())
                return;
              cont.consume(eddy);
            } finally {
              EddyPlugin.getInstance(project).getWidget().lessBusy();
            }
          } catch (InterruptedException e) {
            // interrupted while sleeping, ignore
          }
        }
      });
    } catch (RuntimeInterruptedException e) {
      // interrupted while sleeping inside DumbService, ignore
    }
  }
}
