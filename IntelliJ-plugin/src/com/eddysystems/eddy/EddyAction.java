package com.eddysystems.eddy;

import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import org.jetbrains.annotations.NotNull;
import tarski.Denotations;
import tarski.Scores;

import javax.swing.*;
import java.util.List;

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
      final BaseListPopupStep<scala.Tuple2<Scores.Score,List<Denotations.StmtDen>>> step =
        new BaseListPopupStep<scala.Tuple2<Scores.Score,List<Denotations.StmtDen>>>("eddy suggests:", eddy.getResults()) {
          @Override
          public boolean isAutoSelectionEnabled() {
            return false;
          }

          @Override
          public boolean isSpeedSearchEnabled() {
            return true;
          }

          @Override
          public PopupStep onChosen(scala.Tuple2<Scores.Score,List<Denotations.StmtDen>> selectedValue, boolean finalChoice) {
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
          public boolean hasSubstep(scala.Tuple2<Scores.Score,List<Denotations.StmtDen>> selectedValue) {
            return false;
          }

          @NotNull
          @Override
          public String getTextFor(scala.Tuple2<Scores.Score,List<Denotations.StmtDen>> value) {
            return eddy.code(value);
          }

          @Override
          public Icon getIconFor(scala.Tuple2<Scores.Score,List<Denotations.StmtDen>> value) {
            return null;
          }
        };
      JBPopupFactory.getInstance().createListPopup(step).showInBestPositionFor(eddy.getEditor());

    }
    return true;
  }
}
