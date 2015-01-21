package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.JavaEnvironment;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.project.DumbService;

public class EddyApplicationListener implements ApplicationListener, DumbService.DumbModeListener {

  // ApplicationListenerInterface
  @Override public boolean canExitApplication() { return true; }
  @Override public void applicationExiting() {}
  @Override public void writeActionStarted(Object action) {}
  @Override public void writeActionFinished(Object action) {}

  @Override public void beforeWriteActionStart(Object action) {
    // The eddy thread has a read access token. Release it to let the write action start.
    EddyThread.pause(action);

    // The initialization code has a read access token. Release it to let the write action start.
    JavaEnvironment.writeActionWaiting();
  }

  // DumbModeListener interface
  @Override public void enteredDumbMode() {
    // EddyThread will not restart until dumb mode ends
    EddyThread.pause(null);
  }

  @Override public void exitDumbMode() {}
}
