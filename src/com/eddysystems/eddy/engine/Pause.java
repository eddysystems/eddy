package com.eddysystems.eddy.engine;

import com.intellij.openapi.application.ApplicationManager;

public class Pause {
  // Number of active pauses, and a lock to wait on
  private int pauses = 0;
  private final Object pauseLock = new Object();

  // Pause, returns whether already paused
  public boolean pause() {
    // Increment the pauses counter and register a pause action if we're the first
    final boolean register;
    synchronized (pauseLock) {
      register = pauses == 0;
      pauses++;
    }
    return register;
  }

  public boolean paused() {
    return pauses > 0;
  }

  // Cede control until all pauses complete
  public void waitForEnd() {
    assert !ApplicationManager.getApplication().isReadAccessAllowed();
    try {
      synchronized (pauseLock) {
        while (pauses > 0)
          pauseLock.wait();
      }
    } catch (InterruptedException e) {
      throw new ThreadDeath();
    }
  }

  // Must be called once for each call to pause()
  public void unpause() {
    synchronized (pauseLock) {
      // Decrement the pauses counter and notify if we hit zero.
      assert pauses > 0;
      pauses--;
      if (pauses == 0)
        pauseLock.notify();
    }
  }
}
