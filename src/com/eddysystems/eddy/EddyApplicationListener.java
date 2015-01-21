package com.eddysystems.eddy;

import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.project.DumbService;

public class EddyApplicationListener implements ApplicationListener, DumbService.DumbModeListener {
  // ApplicationListenerInterface
  @Override public boolean canExitApplication() { return true; }
  @Override public void applicationExiting() {}
  @Override public void writeActionStarted(Object action) {}
  @Override public void writeActionFinished(Object action) {}

  @Override public void beforeWriteActionStart(Object action) {
    // The eddy thread runs in a ReadAction.  Kill it to let the write action start.
    EddyThread.pause(action);
  }

  // DumbModeListener interface
  @Override public void enteredDumbMode() {
    // EddyThread will not restart until dumb mode ends
    EddyThread.pause(null);
  }

  @Override public void exitDumbMode() {}
}
