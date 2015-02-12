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
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.ui.Hint;
import com.intellij.ui.LightweightHint;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyFileListener implements CaretListener, DocumentListener {
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final @NotNull Document document;

  private static final @NotNull Object active_lock = new Object();

  // last editor that an eddy thread has run on
  private static Editor active_editor = null;

  // if the eddy instance in a file listener is associated with an active hint, this is it
  private static EddyFileListener active_hint_instance = null;

  // an action that shows up as an intention action (lives longer than the hint)
  private static EddyFileListener active_instance = null;
  private static EddyAction active_action = null;
  private static LightweightHint active_hint = null;
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
    return editor.getCaretModel().getCaretCount() == 1 && EddyPlugin.getInstance(project).isReady();
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
        final double cutoff = Preferences.getData().getNumericMinProbability();
        final double relCutoff = Preferences.getData().getNumericMinRelativeProbability();
        synchronized (active_lock) {
          EddyThread.run(new EddyThread(project,editor,lastEditLocation, new Eddy.Take() {
            @Override public double take(final Eddy.Output output) {
              double thisCutoff = cutoff;
              if (!output.results.isEmpty()) {
                showHint(output);
                // get best probability
                thisCutoff = Math.max(output.results.get(0).p() * relCutoff, cutoff);
              }
              return output.results.size() < 4 ? thisCutoff : 1;
            }
          }));
          active_editor = editor;
        }
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
      if (output.shouldShowHint()) {
        final int offset = editor.getCaretModel().getOffset();
        active_hint = EddyHintLabel.makeHint(output);

        // we can only show hints from the UI thread, so schedule that
        ApplicationManager.getApplication().invokeLater(new Runnable() {
          @Override
          public void run() {
            try {
              // whoever showed the hint last is it
              // the execution order of later-invoked things is the same as the call order, and it's on a single thread, so
              // no synchronization is needed in here
              active_hint_instance = EddyFileListener.this;
              int use_offset = Math.min(offset, editor.getDocument().getTextLength());
              HintManagerImpl.getInstanceImpl().showQuestionHint(editor, use_offset, use_offset, active_hint, action, HintManager.ABOVE);
            } catch (NullPointerException e) {
              // silence null pointer exceptions in HintUtil
            }
          }
        }, new Condition() {
          @Override
          public boolean value(Object o) {
            // do not synchronize this -- we're still in the sync block!
            return action != active_action;
          }
        });
      } else {
        // if we shouldn't show a hint, make sure any old hint is hidden
        final Hint current_hint = active_hint;
        active_hint = null;
        if (current_hint != null) {
          ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
              if (current_hint.isVisible()) {
                current_hint.hide();
              }
            }
          });
        }
      }
    }
  }

  public void nextResult() {
    synchronized (active_lock) {
      if (   active_hint_instance == this
          && active_action.getOutput().nextBestResult())
        showHint(active_action.getOutput());
    }
  }

  public void prevResult() {
    synchronized (active_lock) {
      if (   active_hint_instance == this
          && active_action.getOutput().prevBestResult())
        showHint(active_action.getOutput());
    }
  }

  static public boolean isHintShowing() {
    synchronized (active_lock) {
      return active_hint != null && active_hint.isVisible();
    }
  }

  @Override
  public void caretPositionChanged(CaretEvent e) {
    if (inChange)
      return;

    // TODO: only process if input changed (if we switched statements?)
    if (active_editor != this.editor || // process if current thread is for a different editor
        e.getNewPosition().line != e.getOldPosition().line) // process if we switched lines
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
