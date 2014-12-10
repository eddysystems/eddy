package com.eddysystems.eddy;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;

public class Utility {
  public static void processEvents() {
    if (!ApplicationManager.getApplication().isHeadlessEnvironment()) {
      ApplicationManager.getApplication().invokeLater( new Runnable() { @Override public void run() {
        IdeEventQueue.getInstance().flushQueue();
      }});
    }
  }
}
