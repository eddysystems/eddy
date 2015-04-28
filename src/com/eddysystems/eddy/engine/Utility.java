/* Utility: Miscellaneous utilities */

package com.eddysystems.eddy.engine;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiField;
import com.intellij.psi.PsiMethod;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.indexing.IdFilter;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import tarski.Memory;
import tarski.Memory.OnError;

import java.util.*;

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
    String s = c.getClass().getName() + " (" + c.size() + (c.size() == 1 ? " element)\n" : " elements)\n");
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

  public static <A,B> String mapString(final Map<A,B> m) {
    int max = 0;
    final SortedMap<String,String> tree = new TreeMap<String,String>();
    for (final A x : m.keySet()) {
      final String sx = logString(x);
      final String sy = logString(m.get(x));
      max = Math.max(max, sx.length());
      tree.put(sx, sy);
    }
    String s = m.getClass().getName() + " (" + m.size() + (m.size()==1 ? " element)" : " elements)");
    for (final Map.Entry<String, String> e : tree.entrySet()) {
      final String sx = e.getKey();
      s += "\n" + applyIndent(sx + StringUtils.repeat(" ", max - sx.length()) + " = " + e.getValue(), "  ");
    }
    return s;
  }

  public static String logString(Object obj) {
    return obj == null ? "null"
         : obj instanceof Collection ? collectionString((Collection) obj)
         : obj instanceof Map ? mapString((Map) obj)
         : obj.getClass().isArray() ? arrayString((Object[])obj)
         : obj.toString();
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

  public static OnError onError = new OnError() {
    @Override
    public void error(Memory.Info i, Throwable e) {
      log("failed to log " + i + ": " + e);
    }
  };

  // Doing complicated stuff in processFieldsWithName causes nasty deadlocks if we accidentally
  // call into the Scala plugin.  Instead, we collect the fields into a list and process them afterwards.
  public static boolean safeProcessFieldsWithName(final PsiShortNamesCache cache,
                                                  final @NotNull String name,
                                                  final @NotNull Processor<? super PsiField> processor,
                                                  final @NotNull GlobalSearchScope scope,
                                                  final @Nullable IdFilter filter) {
    final List<PsiField> fields = new ArrayList<PsiField>();
    final Processor<PsiField> quick = new Processor<PsiField>() { @Override public boolean process(final PsiField f) {
      fields.add(f);
      return true;
    }};
    cache.processFieldsWithName(name, quick, scope, filter);
    for (final PsiField f : fields)
      if (!processor.process(f))
        return false;
    return true;
  }

  // Doing complicated stuff in processMethodsWithName causes nasty deadlocks if we accidentally
  // call into the Scala plugin.  Instead, we collect the methods into a list and process them afterwards.
  public static boolean safeProcessMethodsWithName(final PsiShortNamesCache cache,
                                                   final @NotNull String name,
                                                   final @NotNull Processor<? super PsiMethod> processor,
                                                   final @NotNull GlobalSearchScope scope,
                                                   final @Nullable IdFilter filter) {
    final List<PsiMethod> methods = new ArrayList<PsiMethod>();
    final Processor<PsiMethod> quick = new Processor<PsiMethod>() { @Override public boolean process(final PsiMethod m) {
      methods.add(m);
      return true;
    }};
    cache.processMethodsWithName(name, quick, scope, filter);
    for (final PsiMethod m : methods)
      if (!processor.process(m))
        return false;
    return true;
  }
}
