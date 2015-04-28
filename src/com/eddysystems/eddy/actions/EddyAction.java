/* EddyAction: IntelliJ action showing eddy's results
 *
 * This action includes both the single alternative popup showing
 * the best choice and the expanded list showing the others.
 */

package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.engine.Eddy;
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

public class EddyAction {

  public static String getText(final Eddy.Output output) {
    if (output==null || !output.foundSomething())
      return "eddy knows nothing";
    if (output.results.size() == 1)
      return "eddy says: " + output.format(0,abbrevShowFlags());
    else
      return "eddy thinks...";
  }

  public static void execute(final Editor editor, final Eddy.Output output) {
    log("executing EddyAction");

    if (output == null)
      return;
    else if (output.results.size() == 1)
      output.apply(0);
    else if (output.single())
      output.applySelected();
    else {
      class ListEntry {
        final String text;
        final int index;
        ListEntry(String text, int index) { this.text = text; this.index = index; }
      }
      final List<ListEntry> entries = new ArrayList<ListEntry>(output.results.size());
      for (final String s : output.formats(abbrevShowFlags(),true))
        entries.add(new ListEntry(s,entries.size()));

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
  }
}
