package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.codeInsight.hint.QuestionAction;
import com.intellij.ide.DataManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

import static com.eddysystems.eddy.engine.Utility.log;
import static tarski.Tokens.abbrevShowFlags;

public class EddyAction implements QuestionAction {

  private static class ListEntry {
    String text;
    int index;
  }

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
      return "eddy says: " + output.format(0,abbrevShowFlags());
    else
      return "eddy thinks...";
  }

  public @NotNull Eddy.Output getOutput() {
    return output;
  }

  // return how many net characters were inserted (by how much the line has grown/shrunk)
  public int autoExecute() {
    return output.autoApply();
  }

  public boolean isAvailable() {
    return output.foundSomething();
  }

  @Override
  public boolean execute() {
    log("executing EddyAction");

    if (output.results.size() == 1)
      output.apply(0);
    else if (output.single())
      output.applySelected();
    else {

      List<ListEntry> entries = new ArrayList<ListEntry>(output.results.size());

      int i = 0;
      for (final String s : output.formats(abbrevShowFlags(),true)) {
        ListEntry le = new ListEntry();
        le.index = i++;
        le.text = s;
        entries.add(le);
      }

      // show selection dialog
      final BaseListPopupStep<ListEntry> step = new BaseListPopupStep<ListEntry>("eddy thinks:", entries) {
        @Override
        public boolean isAutoSelectionEnabled() {
          return false;
        }

        @Override
        public boolean isSpeedSearchEnabled() {
          return true;
        }

        @Override
        public PopupStep onChosen(final ListEntry selectedValue, final boolean finalChoice) {
          if (selectedValue == null) {
            return FINAL_CHOICE;
          }

          if (finalChoice) {
            output.apply(selectedValue.index);
            return FINAL_CHOICE;
          }

          return FINAL_CHOICE;
        }

        @Override
        public boolean hasSubstep(ListEntry selectedValue) {
          return false;
        }

        @NotNull
        @Override
        public String getTextFor(ListEntry value) {
          return value.text;
        }

        @Override
        public Icon getIconFor(ListEntry value) {
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
