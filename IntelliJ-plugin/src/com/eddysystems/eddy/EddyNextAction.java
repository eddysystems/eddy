package com.eddysystems.eddy;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

import static com.eddysystems.eddy.Utility.log;

public class EddyNextAction extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    log("next action: " + e);
    if (EddyFileListener.activeInstance() != null)
      EddyFileListener.activeInstance().nextResult();
  }
}
