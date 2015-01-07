package com.eddysystems.eddy;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

import static com.eddysystems.eddy.Utility.log;

public class EddyPrevAction extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    log("prev action: " + e);
    if (EddyFileListener.activeInstance() != null)
      EddyFileListener.activeInstance().prevResult();
  }
}
