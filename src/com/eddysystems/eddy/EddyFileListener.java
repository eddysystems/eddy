package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.CaretEvent;
import com.intellij.openapi.editor.event.CaretListener;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.ui.LightweightHint;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyFileListener implements CaretListener, DocumentListener {
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
      current_eddythread = new EddyThread(project,editor,lastEditLocation, new Eddy.Take() {
        @Override public boolean take(Eddy.Output output) {
          showHint(output);
          if (output.results.size() >= 4)
            return true;
          return false;
        }
      });
      current_eddythread_owner = this;
      current_eddythread.start();
    }
  }

  // True if a thread was actually restarted
  public static boolean restartEddyThread() {
    // The eddy thread runs in a ReadAction.  Kill it to let the write action start.
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        killEddyThread(); // does not reset current_eddythread_owner
        current_eddythread_owner.runEddyThread();
        return true;
      }
      return false;
    }
  }

  // True if a thread was actually killed
  public static boolean killEddyThread() {
    // The eddy thread runs in a ReadAction.  Kill it to let the write action start.
    synchronized (current_eddythread_lock) {
      if (current_eddythread != null) {
        current_eddythread.interrupt();
        current_eddythread = null;
        return true;
      }
      return false;
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

  private void showHint(final Eddy.Output output) {
    if (output == null)
      return; // Don't make a hint if there's no output
    final int line = editor.getCaretModel().getLogicalPosition().line;
    final EddyAction action = new EddyAction(output,editor);
    synchronized (active_lock) {
      active_instance = this;
      active_line = line;
      active_action = action;
      active_hint_instance = null;
      // show hint only if we found something really good
      if (output.foundSomethingUseful()) {
        final int offset = editor.getCaretModel().getOffset();
        final LightweightHint hint = EddyHintLabel.makeHint(output);

        // we can only show hints from the UI thread, so schedule that
        ApplicationManager.getApplication().invokeLater(new Runnable() {
          @Override public void run() {
            // whoever showed the hint last is it
            // the execution order of later-invoked things is the same as the call order, and it's on a single thread, so
            // no synchronization is needed in here
            active_hint_instance = EddyFileListener.this;
            HintManagerImpl.getInstanceImpl().showQuestionHint(editor, offset, offset + 1, hint, action, HintManager.ABOVE);
          }
        });
      }
    }
  }

  public void nextResult() {
    synchronized (current_eddythread_lock) {
      if (   current_eddythread_owner == this
          && current_eddythread.output != null
          && current_eddythread.output.nextBestResult())
        showHint(current_eddythread.output);
    }
  }

  public void prevResult() {
    synchronized (current_eddythread_lock) {
      if (   current_eddythread_owner == this
          && current_eddythread.output != null
          && current_eddythread.output.prevBestResult())
        showHint(current_eddythread.output);
    }
  }

  @Override
  public void caretPositionChanged(CaretEvent e) {
    if (inChange)
      return;

    // only process on position change if we switched lines
    // TODO: only process if input changed (ie if we switched statements)
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
}
