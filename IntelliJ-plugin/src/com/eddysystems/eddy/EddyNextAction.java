package com.eddysystems.eddy;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

public class EddyNextAction extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    System.out.println("next action: " + e);
    if (EddyFileListener.activeInstance() != null)
      EddyFileListener.activeInstance().nextResult();
  }
}
