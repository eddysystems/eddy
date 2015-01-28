package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.JavaEnvironment;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.project.DumbService;

import java.util.HashMap;
import java.util.Map;

public class EddyApplicationListener implements ApplicationListener, DumbService.DumbModeListener {

  // we don't want to use the action objects, who knows who's waiting for them. So make our own.
  static final Map<Object, EddyThread.Pause> waiters = new HashMap<Object,EddyThread.Pause>();

  // ApplicationListenerInterface (all these are only called from the dispatch thread
  @Override public boolean canExitApplication() { return true; }
  @Override public void applicationExiting() {}
  @Override public void writeActionStarted(Object action) {
    EddyThread.Pause w;
    synchronized (waiters) {
      w = waiters.get(action);
    }
    if (w != null)
      w.release(); // we're not starting this action, no need to wait for it any more
  }

  @Override public void writeActionFinished(Object action) {}

  @Override public void beforeWriteActionStart(Object action) {
    // The eddy thread has a read access token. Release it to let the write action start.
    // pause all eddy threads and make them give up their read tokens until we notify w
    EddyThread.Pause w = EddyThread.pause(); // returns null if there are no eddy threads to pause
    if (w != null) {
      synchronized (waiters) {
        waiters.put(action,w);
      }
    }

    // The initialization code has a read access token. Release it to let the write action start.
    JavaEnvironment.writeActionWaiting();
  }

  // DumbModeListener interface
  @Override public void enteredDumbMode() {
    // EddyThread will not restart until dumb mode ends
    EddyThread.pause().release();
  }

  @Override public void exitDumbMode() {}
}
