package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Iconable;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiJavaFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class CorrectionIntention implements IntentionAction, Iconable {

  @Override public boolean startInWriteAction() {
    return false;
  }
  @Override @NotNull public String getFamilyName() {
    return "eddy";
  }
  @Override @NotNull public String getText() {
    return "suggest solution";
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, @NotNull PsiFile file) {
    return file instanceof PsiJavaFile && EddyPlugin.getInstance(project).isInitialized();
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, @NotNull PsiFile file) throws IncorrectOperationException {
    // show a dialog that takes a solution. On ok, send to server
    EddyAction action = EddyFileListener.getActionFor(editor);
    CorrectionDialog d = new CorrectionDialog(project,action);
    if (d.showAndGet())
      action.getOutput().logSuggestion(d.getSuggestion());
  }

  @Override
  public Icon getIcon(@IconFlags int flags) {
    return EddyWidget.getIcon();
  }
}