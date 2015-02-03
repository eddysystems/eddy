package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegate;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class EddyEnterHandlerDelegate implements EnterHandlerDelegate {

  @Override
  public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffset, @NotNull Ref<Integer> caretAdvance, @NotNull DataContext dataContext, @Nullable EditorActionHandler originalHandler) {
    if (!Preferences.getData().isAutoApply())
      return Result.Continue;

    // if we have something right now, use it, but don't wait for anything
    EddyAction action = EddyFileListener.getActionFor(editor);
    if (action == null)
      return Result.Continue;

    // only do this if we're at the end of the line
    LogicalPosition pos = editor.getCaretModel().getCurrentCaret().getLogicalPosition();
    int co = editor.getCaretModel().getCurrentCaret().getOffset();
    int nextLine = editor.getDocument().getLineStartOffset(pos.line+1);
    if (!editor.getDocument().getText(new TextRange(co,nextLine)).trim().isEmpty())
      return Result.Continue;

    if (!action.getOutput().shouldAutoApply())
      return Result.Continue;

    Project project = editor.getProject();
    if (project == null)
      return Result.Continue;

    caretOffset.set(caretOffset.get() + action.autoExecute());
    PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument());
    return Result.DefaultSkipIndent;
  }

  @Override
  public Result postProcessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull DataContext dataContext) {
    return Result.Continue;
  }
}
