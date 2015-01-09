package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.IdeActions;
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
import com.intellij.openapi.keymap.KeymapUtil;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyFileListener implements CaretListener, DocumentListener {
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final @NotNull Document document;

  private static final @NotNull Object active_lock = new Object();
  private static EddyFileListener active_instance = null;

  public static EddyFileListener activeInstance() {
    return active_instance;
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

  static class EddyThread extends Thread {
    private final Eddy eddy;
    private final EddyFileListener owner;

    EddyThread(EddyFileListener owner) {
      this.setName("Eddy thread " + getId());
      this.owner = owner;
      this.eddy = new Eddy(owner.project);
    }

    public void interrupt() {
      log("interrupting " + this.getName());
      eddy.cancel();
      super.interrupt();
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

              EddyPlugin.getInstance(owner.project).getWidget().moreBusy();
              eddy.process(owner.editor,owner.getLastEditLocation(),null);
              EddyPlugin.getInstance(owner.project).getWidget().lessBusy();

              if (!eddy.foundSomethingUseful() || isInterrupted())
                return;

              showHint();
            } catch (InterruptedException e) {
              // ignore
            }
          }
        });
      } catch (RuntimeInterruptedException e) {
        // ignore
      }
    }

    protected void showHint() {
      final int offset = owner.editor.getCaretModel().getOffset();
      final String text = eddy.bestText() + (eddy.single() ? " " : " (multiple options...) ");
      final String hintText = text + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(IdeActions.ACTION_SHOW_INTENTION_ACTIONS));

      // we can only show hints from the UI thread, so schedule that
      ApplicationManager.getApplication().invokeLater(new Runnable() {
        @Override public void run() {
          // whoever showed the hint last is it
          synchronized (active_lock) {
            HintManager.getInstance().showQuestionHint(owner.editor, hintText, offset, offset + 1, new EddyAction(eddy));
            active_instance = owner;
          }
        }
      });
    }
  }

  protected void process() {
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
    if (!enabled())
      return;
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
    if (!enabled())
      return;
    process();
  }
}
