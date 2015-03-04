package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.eddysystems.eddy.engine.Eddy;
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

  private Eddy.Output output = null;

  @Override public boolean startInWriteAction() {
    return false; // we'll take care of it inside apply
  }
  @Override @NotNull public String getFamilyName() {
    return "eddy";
  }
  @Override @NotNull public String getText() {
    return EddyAction.getText(output);
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, @NotNull PsiFile file) {
    output = EddyFileListener.getOutputFor(editor);
    return output != null && output.foundSomething();
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, @NotNull PsiFile file) throws IncorrectOperationException {
    if (output != EddyFileListener.getOutputFor(editor)) {
      log("eddy action changed between available and invoke!");
      return;
    }
    EddyAction.execute(editor,output);
  }

  @Override
  public Icon getIcon(@IconFlags int flags) {
    return EddyWidget.getIcon();
  }
}