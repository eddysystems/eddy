package com.eddysystems.eddy;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;

public class Utility {
  public static interface Timed<T> {
    public abstract T call();
  }

  public static <T> T timed(String name, Timed<T> f) {
    long start = System.nanoTime();
    T x = f.call();
    long end = System.nanoTime();
    System.out.println("elapsed "+name+" = "+(end-start)/1e9);
    return x;
  }

  public static void processEvents() {
    if (!ApplicationManager.getApplication().isHeadlessEnvironment()) {
      ApplicationManager.getApplication().invokeLater( new Runnable() { @Override public void run() {
        IdeEventQueue.getInstance().flushQueue();
      }});
    }
  }
}
