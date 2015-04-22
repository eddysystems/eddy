package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Eddy;
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
    return !Preferences.noLog() && file instanceof PsiJavaFile && EddyPlugin.getInstance(project).isInitialized();
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, @NotNull PsiFile file) throws IncorrectOperationException {
    // show a dialog that takes a solution. On ok, send to server
    final Eddy.Output output = EddyFileListener.getOutputFor(editor);
    final CorrectionDialog d = new CorrectionDialog(project,output);
    if (d.showAndGet())
      Eddy.Output.logSuggestion(project, output, d.getSuggestion());
  }

  @Override
  public Icon getIcon(@IconFlags int flags) {
    return EddyWidget.getIcon();
  }
}