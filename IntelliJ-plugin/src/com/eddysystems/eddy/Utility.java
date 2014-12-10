package com.eddysystems.eddy;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;

public class Utility {
  public static interface Timed<T> {
    public abstract T call();
  }

  // State for timeStart and timeStop
  private static String name;
  private static long start;

  public static void timeStart(String name) {
    timeStop();
    Utility.name = name;
    start = System.nanoTime();
  }
  public static void timeStop() {
    final long end = System.nanoTime();
    if (name != null) {
      System.out.println("elapsed "+name+" = "+(end-start)/1e9);
      name = null;
    }
  }

  public static void processEvents() {
    if (!ApplicationManager.getApplication().isHeadlessEnvironment()) {
      ApplicationManager.getApplication().invokeLater( new Runnable() { @Override public void run() {
        IdeEventQueue.getInstance().flushQueue();
      }});
    }
  }
}
