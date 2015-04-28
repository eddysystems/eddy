/* NextSuggestion: Move to the previous suggestion in the list */

package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.EddyFileListener;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

import static com.eddysystems.eddy.engine.Utility.log;

public class PrevSuggestion extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    log("prev action: " + e);
    if (EddyFileListener.activeHintInstance() != null)
      EddyFileListener.activeHintInstance().prevResult();
  }
}
