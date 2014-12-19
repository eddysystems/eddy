package com.eddysystems.eddy;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.codeInsight.intention.BaseElementAtCaretIntentionAction;
import com.intellij.codeInsight.intention.HighPriorityAction;
import com.intellij.codeInspection.HintAction;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.keymap.KeymapUtil;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

public class EddyQuickFix extends BaseElementAtCaretIntentionAction implements HintAction, HighPriorityAction {

  private Eddy eddy;

  public EddyQuickFix(Project project) {
    eddy = new Eddy(project);
  }

  @NotNull
  @Override
  public String getText() {
    return "eddy says: <corrected Java here>";
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return "eddy";
  }

  @Override
  public boolean showHint(@NotNull final Editor editor) {

    // Only if it comes up with something will we show a hint.
    eddy.process(editor,null);

    if (!eddy.foundSomethingUseful())
      return false;

    final QuestionAction action = createEddyAction();

    /*
    // this truly auto-inserts without even asking -- this could be done in a SmartKey thing, but doing it silently is a
    // little harsh because we're potentially still typing
    if (confident()) {
      CommandProcessor.getInstance().runUndoTransparentAction(new Runnable() {
        @Override
        public void run() {
          action.execute();
        }
      });
      return true;
    }
    */

    // show the popup notification

    final String hintText = eddy.bestText() + (eddy.single() ? "" : " (multiple options...)") + " " + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(IdeActions.ACTION_SHOW_INTENTION_ACTIONS));

    int startoffset = editor.getCaretModel().getOffset();
    int endoffset = editor.getCaretModel().getOffset()+1;

    if (!ApplicationManager.getApplication().isUnitTestMode() &&
        !HintManager.getInstance().hasShownHintsThatWillHideByOtherHint(true)) {
      HintManager.getInstance().showQuestionHint(editor, hintText, startoffset, endoffset, action);
    }

    return false;
  }

  protected EddyAction createEddyAction() {
    return new EddyAction(eddy);
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, @NotNull PsiElement element) {
    if (editor.getCaretModel().getCaretCount() != 1)
      return false;

    return eddy.foundSomethingUseful();
  }

  @Override
  public boolean startInWriteAction() {
    return true;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, @NotNull PsiElement element) throws IncorrectOperationException {
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override public void run() {
        createEddyAction().execute();
      }
    });
  }
}