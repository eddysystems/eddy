package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.QuestionAction;
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
  private final @NotNull Eddy.Output output;
  private final @NotNull Editor editor;

  public EddyAction(final @NotNull Eddy.Output output, final Editor editor) {
    this.output = output;
    this.editor = editor;
  }

  public String getText() {
    if (!output.foundSomething())
      return "eddy knows nothing (action)";
    if (output.results.size() == 1)
      return "eddy says: " + output.format(0);
    else
      return "eddy thinks...";
  }

  public boolean isAvailable() {
    return output.foundSomething();
  }

  @Override
  public boolean execute() {
    log("executing EddyAction");

    if (output.single())
      output.applyBest();
    else {
      // show selection dialog
      final BaseListPopupStep<String> step =
        new BaseListPopupStep<String>("eddy thinks:", output.formats()) {
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
              output.apply(selectedValue);
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
        JBPopupFactory.getInstance().createListPopup(step).showInBestPositionFor(editor);
      }});
    }
    return true;
  }
}
