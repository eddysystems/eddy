package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.intellij.codeInsight.intention.HighPriorityAction;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Iconable;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyIntention implements IntentionAction, HighPriorityAction, Iconable {

  private EddyAction action = null;

  @Override public boolean startInWriteAction() {
    return false; // we'll take care of it inside apply
  }
  @Override @NotNull public String getFamilyName() {
    return "eddy";
  }
  @Override @NotNull public String getText() {
    if (action == null)
      return "eddy knows nothing.";
    else
      return action.getText();
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, @NotNull PsiFile file) {
    action = EddyFileListener.getActionFor(editor);
    if (action == null)
      return false;
    else
      return true;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, @NotNull PsiFile file) throws IncorrectOperationException {
    if (action != EddyFileListener.getActionFor(editor)) {
      log("eddy action changed between available and invoke!");
      return;
    }
    action.execute();
  }

  @Override
  public Icon getIcon(@IconFlags int flags) {
    return EddyWidget.getIcon();
  }
}