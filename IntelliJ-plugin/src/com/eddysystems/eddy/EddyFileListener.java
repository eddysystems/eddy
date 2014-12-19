package com.eddysystems.eddy;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
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
import com.intellij.psi.PsiFile;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

public class EddyFileListener implements CaretListener, DocumentListener {
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final @NotNull Document document;
  private final @NotNull PsiFile psifile;
  private static final @NotNull Logger logger = Logger.getInstance("EddyFileListener");

  private static final @NotNull Object active_lock = new Object();
  private static EddyFileListener active_instance = null;

  public static EddyFileListener activeInstance() {
    return active_instance;
  }

  private boolean inChange = false;

  public EddyFileListener(@NotNull Project project, TextEditor editor, @NotNull PsiFile psifile) {
    logger.setLevel(Level.INFO);

    this.project = project;
    this.editor = editor.getEditor();
    this.document = this.editor.getDocument();
    this.psifile = psifile;

    VirtualFile file = FileDocumentManager.getInstance().getFile(this.document);
    if (file != null)
      logger.debug("making eddy for editor for file " + file.getPresentableName());
    else
      logger.debug("making eddy for editor for file 'null'");

    // moving the caret around
    this.editor.getCaretModel().addCaretListener(this);

    // subscribe to document events
    this.editor.getDocument().addDocumentListener(this);
  }

  public void dispose() {
    editor.getCaretModel().removeCaretListener(this);
    editor.getDocument().removeDocumentListener(this);
  }

  protected boolean enabled() {
    return editor.getCaretModel().getCaretCount() == 1 && EddyPlugin.getInstance(project).isInitialized();
  }

  private static final Object current_eddythread_lock = new Object();
  private static EddyFileListener current_eddythread_owner = null;
  private static EddyThread current_eddythread= null;

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
      System.out.println("interrupting " + this.getName());
      eddy.cancel();
      super.interrupt();
    }

    @Override
    public void run() {
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
          } catch (InterruptedException e) {
            return;
          }

          EddyPlugin.getInstance(owner.project).getWidget().moreBusy();
          eddy.process(owner.editor,null);
          EddyPlugin.getInstance(owner.project).getWidget().lessBusy();

          if (!eddy.foundSomethingUseful() || isInterrupted())
            return;

          showHint();
        }
      });
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
            logger.info("made EddyAction for eddy@" + eddy.hashCode());
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
    logger.debug("caret position changed");
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
    logger.debug("before document change");
    inChange = true;
  }

  @Override
  public void documentChanged(DocumentEvent event) {
    logger.debug("document changed");
    inChange = false;
    process();
  }
}
