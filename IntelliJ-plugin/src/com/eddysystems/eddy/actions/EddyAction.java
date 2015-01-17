package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.ide.DataManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyAction implements QuestionAction {

  private final @NotNull Eddy eddy;
  private final @NotNull Editor editor;
  private final @NotNull TextRange replace_range;

  public EddyAction(Eddy eddy) {
    this.eddy = eddy;
    this.editor = eddy.getEditor();
    this.replace_range = eddy.getRange();
  }

  public String getText() {
    if (eddy.getResultStrings() == null || eddy.getResultStrings().isEmpty())
      return "eddy knows nothing.";
    if (eddy.getResultStrings().size() == 1) {
      return "eddy says: " + eddy.getResultStrings().get(0);
    } else {
      return "eddy thinks...";
    }
  }

  @Override
  public boolean execute() {
    log("executing EddyAction");

    if (eddy.single()) {
      eddy.applyBest();
    } else {
      // show selection dialog
      final BaseListPopupStep<String> step =
        new BaseListPopupStep<String>("eddy thinks:", eddy.getResultStrings()) {
          @Override
          public boolean isAutoSelectionEnabled() {
            return false;
          }

          @Override
          public boolean isSpeedSearchEnabled() {
            return true;
          }

          @Override
          public PopupStep onChosen(String selectedValue, boolean finalChoice) {
            if (selectedValue == null) {
              return FINAL_CHOICE;
            }

            if (finalChoice) {
              eddy.apply(selectedValue, editor, replace_range);
              return FINAL_CHOICE;
            }

            return FINAL_CHOICE;
          }

          @Override
          public boolean hasSubstep(String selectedValue) {
            return false;
          }

          @NotNull
          @Override
          public String getTextFor(String value) {
            return value;
          }

          @Override
          public Icon getIconFor(String value) {
            return null;
          }
        };
      DataManager.getInstance().getDataContextFromFocus().doWhenDone(new Runnable() { @Override public void run() {
        JBPopupFactory.getInstance().createListPopup(step).showInBestPositionFor(eddy.getEditor());
      }});
    }
    return true;
  }
}
