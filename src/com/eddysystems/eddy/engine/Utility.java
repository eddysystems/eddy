package com.eddysystems.eddy.engine;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import org.apache.log4j.Level;

import java.util.Collection;
import java.util.Stack;

public class Utility {
  static long last_queue_process_events = System.nanoTime();

  // WARNING: this class may mess with stuff things if used inside of read actions. Meant to be used where
  // the read action mechanism is not fine grained enough
  public static class SmartReadLock {
    private final Project project;
    private final DumbService ds;

    private final Object lock = new Object();
    private AccessToken token = null;

    SmartReadLock(Project project) {
      this.project = project;
      this.ds = DumbService.getInstance(project);
    }

    // locks until we're inside a read action, and we are in smart mode
    public synchronized void acquire() {
      // we have read access and we're not dumb, great.
      if (token != null && !ds.isDumb())
        return;

      // if we have a token, but we're in dumb mode, release the token, and then get a new one once
      // dumb mode ends
      if (token != null)
        release();

      // wait for smart mode and get a new access token.
      while (true) {
        ds.waitForSmartMode();

        // get a new read access token
        synchronized (lock) {
          token = ApplicationManager.getApplication().acquireReadActionLock();
        }

        // did we become dumb?
        if (!ds.isDumb()) {
          break;
        } else {
          // release lock and try again
          release();
        }
      }
      assert ApplicationManager.getApplication().isReadAccessAllowed();
    }

    public boolean locked() {
      return token != null;
    }

    // release the read lock if we have it, return whether we had it
    public synchronized boolean release() {
      if (token != null) {
        token.finish();
        token = null;
        return true;
      } else
        return false;
    }
  }

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

  public static String applyIndent(String s, String indent) {
    return indent + s.replaceAll("\n", "\n" + indent);
  }

  public static <A> String arrayString(A[] a) {
    String s = a.getClass().getName() + " (" + a.length + " elements):\n";
    int i = 0;
    for (A it : a) {
      if (i++ > 10) {
        log("  ...\n");
        break;
      }
      s += applyIndent(logString(it), "  ") + '\n';
    }
    return s;
  }

  public static <A> String collectionString(Collection<A> c) {
    String s = c.getClass().getName() + " (" + c.size() + " elements):\n";
    int i = 0;
    for (A it : c) {
      if (i++ > 10) {
        s += "  ...\n";
        break;
      }
      s += applyIndent(logString(it), "  ") + '\n';
    }
    return s;
  }

  public static String logString(Object obj) {
    // special handling for collections and arrays
    if (obj instanceof Collection<?>) {
      return collectionString((Collection<Object>)obj);
    } else if (obj == null) {
      return "null";
    } else if (obj.getClass().isArray()) {
      return arrayString((Object[])obj);
    } else {
      // ls is a regular object
      return obj.toString();
    }
  }

  // can't use logger in test mode. Sucks.
  public static void log(Logger logger, Level level, String msg) {
    if (logger == null || ApplicationManager.getApplication().isHeadlessEnvironment())
      System.out.println(msg);
    else if (level == Level.INFO)
      logger.info(msg);
    else if (level == Level.DEBUG)
      logger.debug(msg);
    else if (level == Level.WARN)
      logger.warn(msg);
    else if (level == Level.ERROR)
      logger.error(msg);
    else
      logger.info(msg);
  }

  private static Logger logger = Logger.getInstance("Eddy");
  public static void log(Object msg) {
    log(logger, Level.INFO, logString(msg));
  }
  public static void log(String msg) {
    log(logger, Level.INFO, msg);
  }

  public static void logConsole(Object msg) {
    log(null, Level.INFO, logString(msg));
  }
  public static void logConsole(String msg) {
    log(null, Level.INFO, msg);
  }

  public static void logError(String where, Throwable e) {
    log("exception "+e+" in "+where+": "+e.getMessage());
    log("trace: ");
    log(e.getStackTrace());
  }
}
