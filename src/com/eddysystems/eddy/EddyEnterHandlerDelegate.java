package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegate;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class EddyEnterHandlerDelegate implements EnterHandlerDelegate {

  @Override
  public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffset, @NotNull Ref<Integer> caretAdvance, @NotNull DataContext dataContext, @Nullable EditorActionHandler originalHandler) {
    // if we have something right now, use it, but don't wait for anything
    final Eddy.Output output = EddyFileListener.getOutputFor(editor);
    if (output == null)
      return Result.Continue;

    // only do this if a hint is showing
    if (!EddyFileListener.isHintShowing()) {
      return Result.Continue;
    }

    // check if we should be auto-applying
    if (!output.shouldAutoApply())
      return Result.Continue;

    Project project = editor.getProject();
    if (project == null)
      return Result.Continue;

    int co = editor.getCaretModel().getCurrentCaret().getOffset();
    // autoExecute will commit the document
    caretOffset.set(caretOffset.get() - co + output.autoApply());
    return Result.DefaultSkipIndent;
  }

  @Override
  public Result postProcessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull DataContext dataContext) {
    return Result.Continue;
  }
}
