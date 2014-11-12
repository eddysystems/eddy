package com.eddysystems.eddy;

import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class EddyAction implements QuestionAction {

  private Eddy eddy;

  EddyAction(Eddy eddy) {
    this.eddy = eddy;
  }

  @Override
  public boolean execute() {
    System.out.println("executing EddyAction");

    if (eddy.single()) {
      eddy.applyBest();
    } else {
      // show selection dialog
      final BaseListPopupStep<String> step =
        new BaseListPopupStep<String>("eddy suggests:", eddy.getResultStrings()) {
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
              eddy.apply(selectedValue);
              return FINAL_CHOICE;
            }

            return FINAL_CHOICE;

            /*
            List<String> toExclude = getAllExcludableStrings(qname);
            return new BaseListPopupStep<String>(null, toExclude) {
              @NotNull
              @Override
              public String getTextFor(String value) {
                return "Exclude '" + value + "' from auto-import";
              }

              @Override
              public PopupStep onChosen(String selectedValue, boolean finalChoice) {
                if (finalChoice) {
                  excludeFromImport(myProject, selectedValue);
                }

                return super.onChosen(selectedValue, finalChoice);
              }
            };
            */
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
