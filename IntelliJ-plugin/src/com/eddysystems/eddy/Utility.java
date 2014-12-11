package com.eddysystems.eddy;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;

public class Utility {
  static long last_queue_process_events = System.nanoTime();

  public static void processEvents() {
    if (!ApplicationManager.getApplication().isHeadlessEnvironment()) {
      long time = System.nanoTime();

      // don't queue more often than once every 100ms
      if (time - last_queue_process_events > 1e8) {
        last_queue_process_events = time;
        ApplicationManager.getApplication().invokeLater( new Runnable() { @Override public void run() {
          IdeEventQueue.getInstance().flushQueue();
        }});
      }
    }
  }
}
