/* EddyApplicationListener: Pause the eddy thread to let write actions go through */

package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.JavaEnvironment;
import com.intellij.openapi.application.ApplicationListener;
import com.intellij.openapi.project.DumbService;

public class EddyApplicationListener implements ApplicationListener, DumbService.DumbModeListener {

  // ApplicationListenerInterface (all these are only called from the dispatch thread
  @Override public boolean canExitApplication() { return true; }
  @Override public void applicationExiting() {}
  @Override public void writeActionStarted(Object action) {
    EddyThread.unpause();
    JavaEnvironment.pause.unpause();
  }

  @Override public void writeActionFinished(Object action) {}

  @Override public void beforeWriteActionStart(Object action) {
    // The eddy thread has a read access token. Release it to let the write action start.
    // pause all eddy threads and make them give up their read tokens until we notify.
    EddyThread.doPause();

    // The initialization code has a read access token. Release it to let the write action start.
    JavaEnvironment.pause.pause();
  }

  // DumbModeListener interface
  @Override public void enteredDumbMode() {
    // EddyThread will not restart until dumb mode ends
    EddyThread.doPause();
    EddyThread.unpause();
  }

  @Override public void exitDumbMode() {}
}
