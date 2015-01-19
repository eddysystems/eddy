package com.eddysystems.eddy;

import com.intellij.openapi.application.ApplicationListener;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyApplicationListener implements ApplicationListener {
  @Override public boolean canExitApplication() { return true; }
  @Override public void applicationExiting() {}
  @Override public void writeActionStarted(Object action) {}
  @Override public void writeActionFinished(Object action) {}

  @Override public void beforeWriteActionStart(Object action) {
    // The eddy thread runs in a ReadAction.  Kill it to let the write action start.
    if (EddyFileListener.restartEddyThread()) {
      log("Restarting eddy thread to allow write action:");
      log(action);
    }
  }
}
