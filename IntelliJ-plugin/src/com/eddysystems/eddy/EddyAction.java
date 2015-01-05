package com.eddysystems.eddy;

import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import static com.eddysystems.eddy.Utility.log;

public class EddyAction implements QuestionAction {

  private Eddy eddy;
  private final @NotNull Editor editor;
  private final @NotNull TextRange replace_range;

  EddyAction(Eddy eddy) {
    this.eddy = eddy;
    this.editor = eddy.getEditor();
    this.replace_range = eddy.getRange();
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
              Eddy.apply(selectedValue, editor, replace_range);
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
      JBPopupFactory.getInstance().createListPopup(step).showInBestPositionFor(eddy.getEditor());
    }
    return true;
  }
}
