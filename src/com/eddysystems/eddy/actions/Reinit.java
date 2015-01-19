package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.EddyPlugin;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

public class Reinit extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    EddyPlugin.getInstance(e.getProject()).dropEnv();
    EddyPlugin.getInstance(e.getProject()).requestInit();
  }
}
