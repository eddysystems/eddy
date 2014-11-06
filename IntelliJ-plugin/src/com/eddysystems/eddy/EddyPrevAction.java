package com.eddysystems.eddy;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

public class EddyPrevAction extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    System.out.println("prev action: " + e);
    if (EddyFileListener.activeInstance() != null)
      EddyFileListener.activeInstance().prevResult();
  }
}
