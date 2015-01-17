package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.CaretEvent;
import com.intellij.openapi.editor.event.CaretListener;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.ui.LightweightHint;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyFileListener implements CaretListener, DocumentListener, ApplicationListener {
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final @NotNull Document document;

  private static final @NotNull Object active_lock = new Object();

  // if the eddy instance in a file listener is associated with an active hint, this is it
  private static EddyFileListener active_hint_instance = null;

  // an action that shows up as an intention action (lives longer than the hint)
  private static EddyFileListener active_instance = null;
  private static EddyAction active_action = null;
  private static int active_line = -1;

  // to keymapped actions that should affect the file listener that showed the last hint
  public static EddyFileListener activeHintInstance() {
    return active_hint_instance;
  }

  public static EddyAction getActionFor(Editor editor) {
    synchronized (active_lock) {
      if (active_instance == null || editor != active_instance.editor ||
          editor.getCaretModel().getCurrentCaret().getLogicalPosition().line != active_line)
        return null;
      return active_action;
    }
  }

  private boolean inChange = false;
  private int lastEditLocation = -1;

  public int getLastEditLocation() {
    return lastEditLocation;
  }

  public EddyFileListener(@NotNull Project project, TextEditor editor) {
    this.project = project;
    this.editor = editor.getEditor();
    this.document = this.editor.getDocument();

    VirtualFile file = FileDocumentManager.getInstance().getFile(this.document);
    if (file != null)
      log("making eddy for editor for file " + file.getPresentableName());
    else
      log("making eddy for editor for file 'null'");

    // moving the caret around
    this.editor.getCaretModel().addCaretListener(this);

    // subscribe to document events
    this.editor.getDocument().addDocumentListener(this);
  }

  public void dispose() {
    log("disposing editor listener for " + editor);
    editor.getCaretModel().removeCaretListener(this);
    editor.getDocument().removeDocumentListener(this);
  }

  protected boolean enabled() {
    return editor.getCaretModel().getCaretCount() == 1 && EddyPlugin.getInstance(project).isInitialized();
  }

  private static final Object current_eddythread_lock = new Object();
  private static EddyFileListener current_eddythread_owner = null;
  private static EddyThread current_eddythread = null;

  private void runEddyThread() {
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        current_eddythread.interrupt();
      }
      current_eddythread = new EddyThread(this);
      current_eddythread_owner = this;
      current_eddythread.start();
    }
  }

  public static EddyThread getEddyThread() {
    Thread t = Thread.currentThread();
    if (t instanceof EddyThread)
      return (EddyThread)t;
    else
      return null;
  }

  public static class EddyThread extends Thread {
    private final Eddy eddy;
    private final EddyFileListener owner;

    EddyThread(EddyFileListener owner) {
      this.setName("Eddy thread " + getId());
      this.owner = owner;
      this.eddy = new Eddy(owner.project);
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
        DumbService.getInstance(owner.project).runReadActionInSmartMode(new Runnable() {
            @Override
            public void run() {
            // if we waited for smart mode, don't wait too much longer
            try {
              int millis = new Double(200-(System.nanoTime()-start)/1e3).intValue();
              if (millis > 0)
                sleep(millis);

              try {
                EddyPlugin.getInstance(owner.project).getWidget().moreBusy();

                eddy.process(owner.editor,owner.getLastEditLocation(),null);
                if (isInterrupted())
                  return;
                showHint();
              } finally {
                EddyPlugin.getInstance(owner.project).getWidget().lessBusy();
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

    protected void showHint() {
      final int line = owner.editor.getCaretModel().getLogicalPosition().line;
      final EddyAction action = new EddyAction(eddy);
      synchronized (active_lock) {
        active_instance = owner;
        active_line = line;
        active_action = action;
        active_hint_instance = null;
        // show hint
        if (eddy.foundSomethingUseful()) {
          final int offset = owner.editor.getCaretModel().getOffset();
          final LightweightHint hint = EddyHintLabel.makeHint(eddy);

          // we can only show hints from the UI thread, so schedule that
          ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override public void run() {
              // whoever showed the hint last is it
              // the execution order of later-invoked things is the same as the call order, and it's on a single thread, so
              // no synchronization is needed in here
              active_hint_instance = owner;
              HintManagerImpl.getInstanceImpl().showQuestionHint(owner.editor, offset, offset + 1, hint, action, HintManager.ABOVE);
            }
          });
        }
      }
    }
  }

  protected void process() {
    if (!enabled()) {
      // check if we're not initialized, and if so, try to reinitialize
      if (!EddyPlugin.getInstance(project).isInitialized()) {
        EddyPlugin.getInstance(project).requestInit();
      }
      return;
    }

    PsiDocumentManager.getInstance(project).performForCommittedDocument(document, new Runnable() {
      @Override
      public void run() {
        runEddyThread();
      }
    });
  }

  public void nextResult() {
    synchronized (current_eddythread_lock) {
      if (current_eddythread_owner == this) {
        if (current_eddythread.eddy.nextBestResult())
          current_eddythread.showHint();
      }
    }
  }

  public void prevResult() {
    synchronized (current_eddythread_lock) {
      if (current_eddythread_owner == this) {
        if (current_eddythread.eddy.prevBestResult())
          current_eddythread.showHint();
      }
    }
  }

  public void dumpEnvironment(String filename) {
    synchronized (current_eddythread_lock) {
      if (current_eddythread_owner == this) {
        current_eddythread.eddy.dumpEnvironment(filename);
      }
    }
  }

  @Override
  public void caretPositionChanged(CaretEvent e) {
    if (inChange)
      return;

    // only process on position change if we switched lines
    if (e.getNewPosition().line != e.getOldPosition().line)
      process();
  }

  @Override
  public void caretAdded(CaretEvent e) {
  }

  @Override
  public void caretRemoved(CaretEvent e) {
  }

  @Override
  public void beforeDocumentChange(DocumentEvent event) {
    inChange = true;
  }

  @Override
  public void documentChanged(DocumentEvent event) {
    inChange = false;
    lastEditLocation = event.getOffset();
    process();
  }

  // ApplicationListener methods
  @Override public boolean canExitApplication() { return true; }
  @Override public void applicationExiting() {}
  @Override public void writeActionStarted(Object action) {}
  @Override public void writeActionFinished(Object action) {}

  @Override public void beforeWriteActionStart(Object action) {
    // The eddy thread runs in a ReadAction.  Kill it to let the write action start.
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        current_eddythread.interrupt();
        current_eddythread = null;
        log("Killing eddy thread to allow write action:");
        log(action);
      }
    }
  }
}
