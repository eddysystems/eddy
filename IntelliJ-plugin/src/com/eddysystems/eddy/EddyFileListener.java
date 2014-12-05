package com.eddysystems.eddy;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.IdeActions;
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
  private final @NotNull Logger logger = Logger.getInstance(getClass());
  private final @NotNull Eddy eddy = new Eddy();

  private static EddyFileListener active = null;
  private static final Object activeLock = new Object();

  private boolean inChange = false;

  public static EddyFileListener activeInstance() {
    return active;
  }

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
    // wait till eddy is reddy, and only with a single caret can we deal...
    return eddy.ready() && editor.getCaretModel().getCaretCount() == 1;
  }

  protected void showHint() {
    int offset = editor.getCaretModel().getOffset();
    String text = eddy.bestText() + (eddy.single() ? " " : " (multiple options...) ");
    String hintText = text + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(IdeActions.ACTION_SHOW_INTENTION_ACTIONS));

    // whoever showed the hint last is it
    synchronized(activeLock) {
      HintManager.getInstance().showQuestionHint(editor, hintText, offset, offset + 1, new EddyAction(eddy));
      active = this;
    }
  }

  protected void process() {
    PsiDocumentManager.getInstance(project).performForCommittedDocument(document, new Runnable() {
      @Override
      public void run() {
        eddy.process(editor);

        if (!eddy.foundSomethingUseful())
          return;

        showHint();
      }
    });
  }

  public void nextResult() {
    if (eddy.nextBestResult()) {
      showHint();
    }
  }

  public void prevResult() {
    if (eddy.prevBestResult()) {
      showHint();
    }
  }

  public void dumpEnvironment(String filename) {
    eddy.dumpEnvironment(filename);
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
