package com.eddysystems.eddy.engine;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import org.apache.log4j.Level;

import java.util.Collection;

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
    if (ApplicationManager.getApplication().isHeadlessEnvironment())
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

  public static void logError(String where, Throwable e) {
    log("exception "+e+" in "+where+": "+e.getMessage());
    log("trace: ");
    log(e.getStackTrace());
  }

  // Are we in debug mode?
  private static int _isDebug = 2; // 0 for no, 1 for yes, 2 for uninitialized
  public static boolean isDebug() {
    if (_isDebug == 2) {
      // See https://stackoverflow.com/questions/3776204/how-to-find-out-if-debug-mode-is-enabled
      _isDebug = java.lang.management.ManagementFactory.getRuntimeMXBean()
                   .getInputArguments().toString().contains("jdwp") ? 1 : 0;
    }
    return _isDebug == 1;
  }
}
