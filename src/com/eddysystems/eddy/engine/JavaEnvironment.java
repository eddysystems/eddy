package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.*;
import com.intellij.psi.impl.java.stubs.index.JavaFullClassNameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.stubs.StubIndexImpl;
import com.intellij.util.Processor;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import tarski.*;

import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.locks.Lock;

import static com.eddysystems.eddy.engine.Utility.log;
import static com.eddysystems.eddy.engine.Utility.logError;
import static java.lang.Thread.sleep;
import static utility.JavaUtils.popScope;
import static utility.JavaUtils.pushScope;

// a class storing information about the environment.
public class JavaEnvironment {

  public static class NoJDKError extends RuntimeException {
    NoJDKError(String s) {
      super("No JDK found: " + s);
    }
  }

  @NotNull final Project project;

  private boolean _initialized = false;
  public boolean initialized() {
    return _initialized;
  }

  // pre-computed data structures to enable fast creation of appropriate scala Env instances

  // these may be overwritten any time by new objects created in an updating process, but they are never changed.
  // Grabbing a copy and working with it is always safe even without a lock

  // a trie encapsulating the global list of all names. This is never edited, so it's safe to use without a lock
  private int[] nameTrie = null;

  // map types to items of that type. Use only with the lock in hand.
  private final Object byItemLock = new Object();
  private Map<String, Set<String>> pByItem = null;

  // the background update thread
  private Future<?> updateFuture = null;
  private boolean needUpdate = false;

  public JavaEnvironment(@NotNull Project project) {
    this.project = project;
  }

  // make sure our background updater is done before we disappear
  public void dispose() {
    updateFuture.cancel(true);
    try {
      // wait for background thread to exit
      updateFuture.get();
    } catch (CancellationException e) {
      // we were cancelled, no shit
    } catch (InterruptedException e) {
      // we exited, that's what matters
    } catch (ExecutionException e) {
      // we exited, that's what matters
    }
  }

  // TODO: add a single field to valuesByItem (should not have to rebuild completely all the time) -- requires lock!

  // request a full update of the global lookups
  public void requestUpdate() {
    requestUpdate(null);
  }

  // for initialization
  public synchronized void updateSync(@Nullable final ProgressIndicator indicator) throws ExecutionException, InterruptedException {
    requestUpdate(indicator);
    updateFuture.get();
  }


  private synchronized void requestUpdate(@Nullable final ProgressIndicator indicator) {
    needUpdate = true;
    if (updateFuture == null || updateFuture.isDone()) {
      scheduleUpdate(indicator);
    }
  }

  private synchronized void scheduleUpdate(@Nullable final ProgressIndicator indicator) {
    needUpdate = false;
    updateFuture = ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
      @Override
      public void run() {
        backgroundUpdate(indicator);
      }
    });
  }

  private void backgroundUpdate(@Nullable final ProgressIndicator indicator) {
    pushScope("update environment");
    try {
      // call this once so initialization fails if no JDK is present
      if (!_initialized) {
        // add base items
        DumbService.getInstance(project).runReadActionInSmartMode(new Runnable() {
          @Override
          public void run() {
            addBase(new Converter(project, new HashMap<PsiElement, Items.Item>()));
          }
        });
      }

      // make global name lookup trie (read actions taken care of inside)
      String[] fieldNames = DumbService.getInstance(project).runReadActionInSmartMode( new Computable<String[]>() { @Override public String[] compute() {
        return PsiShortNamesCache.getInstance(project).getAllFieldNames();
      }});

      nameTrie = prepareNameTrie(fieldNames);

      // we're initialized starting here, we can live without the makeProjectValuesByItem for a while
      _initialized = true;

      if (indicator != null)
        indicator.setIndeterminate(false);

      try {
        // takes care of read action business inside (this is where all the work happens)
        synchronized (byItemLock) {
          pByItem = makeProjectValuesByItem(fieldNames, indicator);
        }
      } finally {
        if (indicator != null)
          indicator.setIndeterminate(true);
      }

    } finally { popScope(); }

    // schedule the next update immediately if we've had another update request while we
    // were in here
    if (needUpdate)
      scheduleUpdate(null);
  }

  private static void addBase(final Converter converter) {
    pushScope("add base");
    try {
      final GlobalSearchScope scope = ProjectScope.getAllScope(converter.project);

      // Extra things don't correspond to PsiElements
      final Set<Items.Item> extra =  new HashSet<Items.Item>();
      Collections.addAll(extra, Base.extraItems());

      // Add classes and packages
      final JavaPsiFacade facade = JavaPsiFacade.getInstance(converter.project);
      for (Items.Item item : tarski.Base.baseItems()) {
        if (extra.contains(item) || item instanceof Items.ConstructorItem)
          continue;
        final String name = item.qualified();
        PsiElement psi;
        if (item instanceof Items.Package)
          psi = facade.findPackage(name);
        else if (item instanceof Items.ClassItem)
          psi = facade.findClass(name,scope);
        else
          throw new NotImplementedError("Unknown base type "+item.getClass());
        if (psi == null)
          throw new NoJDKError("Couldn't find " + name);
        //log("adding base item " + item + " for " + psi + "@" + psi.hashCode() + " original " + psi.getOriginalElement().hashCode());

        if (!converter.knows(psi))
          converter.put(psi,item);
      }

      // Add constructors
      for (Items.Item item : tarski.Base.baseItems()) {
        if (!(item instanceof Items.ConstructorItem))
          continue;
        final String clsName = ((Items.ConstructorItem)item).parent().qualified();
        final PsiClass cls = facade.findClass(clsName,scope);
        assert cls != null;
        final PsiMethod[] cons = cls.getConstructors();
        if (cons.length != 1)
          log("found " + cons.length + " constructors for object " + cls);
        if (!converter.knows(cons[0]))
          converter.put(cons[0],item);
      }

      // Add class members
      for (Items.Item item : tarski.Base.baseItems()) {
        if (extra.contains(item) || !(item instanceof Items.ClassItem))
          continue;
        final String name = item.qualified();
        converter.addClassMembers(facade.findClass(name, scope), (Items.ClassItem) item);
       }
    } finally { popScope(); }
  }

  private int[] prepareNameTrie(String[] fieldNames) {
    pushScope("prepare lazy trie");
    try {
      if (updateFuture.isCancelled())
        return null;

      String[] classNames = DumbService.getInstance(project).runReadActionInSmartMode( new Computable<String[]>() { @Override public String[] compute() {
        return PsiShortNamesCache.getInstance(project).getAllClassNames();
      }});

      if (updateFuture.isCancelled())
        return null;

      String[] methodNames = DumbService.getInstance(project).runReadActionInSmartMode( new Computable<String[]>() { @Override public String[] compute() {
        return PsiShortNamesCache.getInstance(project).getAllMethodNames();
      }});

      if (updateFuture.isCancelled())
        return null;

      String[] allNames = new String[classNames.length + fieldNames.length + methodNames.length];
      System.arraycopy(classNames, 0, allNames, 0, classNames.length);
      System.arraycopy(fieldNames, 0, allNames, classNames.length, fieldNames.length);
      System.arraycopy(methodNames, 0, allNames, classNames.length+fieldNames.length, methodNames.length);

      if (updateFuture.isCancelled())
        return null;

      // there may be duplicates, but we don't particularly care
      Arrays.sort(allNames);
      return JavaTrie.makeTrieStructure(allNames);
    } finally { popScope(); }
  }

  // TODO: this should use the same pause mechanism as EddyThread (counted)
  private static boolean _writeActionWaiting = false;
  public static synchronized void writeActionWaiting() {
    _writeActionWaiting = true;
  }
  private static synchronized boolean checkClearWriteActionWaiting() {
    if (_writeActionWaiting) {
      _writeActionWaiting = false;
      return true;
    } else
      return false;
  }

  private Map<String, Set<String>> makeProjectValuesByItem(String[] fieldNames, @Nullable ProgressIndicator indicator) {
    pushScope("make project values by item");

    // Prepare to grab index write locks to avoid crazy deadlocks.  This is a terrible hack around broken OPC.
    final Lock fqnLock = ((StubIndexImpl)StubIndex.getInstance())
      .getWriteLock(JavaFullClassNameIndex.getInstance().getKey());

    try {
      final Map<String,Set<String>> result = new HashMap<String,Set<String>>();

      final GlobalSearchScope scope = ProjectScope.getContentScope(project);
      final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
      final IdFilter filter = IdFilter.getProjectIdFilter(project, false);
      final Processor<PsiField> proc = new Processor<PsiField>() {

        final Stack<PsiType> work = new Stack<PsiType>();
        final Set<PsiType> seen = new HashSet<PsiType>();

        @Override
        public boolean process(final PsiField f) {
          // check if we've been cancelled
          if (updateFuture.isCancelled())
            return false;

          fqnLock.lock();
          try {
            // put this field into the string map for its type and all its supertypes
            String name = f.getName();
            seen.clear();
            work.clear();
            work.push(f.getType());
            while (!work.isEmpty()) {
              PsiType t = work.pop();
              // we don't want generics in here
              if (t instanceof PsiClassType) {
                // never add java.lang.Object
                if (((PsiClassType)t).getClassName().equals("Object")) {
                  PsiClass tc = ((PsiClassType)t).resolve();
                  if (tc != null && "java.lang.Object".equals(tc.getQualifiedName()))
                      continue;
                }
                t = ((PsiClassType)t).rawType();
              }

              // add to map
              String type = t.getCanonicalText();
              //log("found field " + f + " with type " + type);
              Set<String> values = result.get(type);
              if (values == null) {
                values = new HashSet<String>();
                result.put(type,values);
              }
              values.add(name);

              for (final PsiType s : t.getSuperTypes()) {
                if (!seen.contains(s)) {
                  seen.add(s);
                  work.push(s);
                }
              }
            }

          } catch (AssertionError e) {
            // If we're in the Scala plugin, log and squash the error.  Otherwise, rethrow.
            if (utility.Utility.fromScalaPlugin(e)) logError("makeProjectValuesByItem()",e);
            else throw e;
          } finally {
            fqnLock.unlock();
          }
          return true;
        }
      };

      final Utility.SmartReadLock lock = new Utility.SmartReadLock(project);
      int i = 0;
      final double n = fieldNames.length;
      try {
        for (final String s : fieldNames) {
          if (indicator != null) {
            indicator.checkCanceled();
            indicator.setFraction(i++/n);
          }

          // pretend we're doing a big read action here, if we get stumped by a dumb mode, repeat until it passes
          boolean done = false;
          while (!done) {
            done = true;
            lock.acquire();
            try {
              cache.processFieldsWithName(s, proc, scope, filter);
            } catch (AssertionError e) {
              // If we're in the Scala plugin, log and squash the error. Don't retry. Otherwise, rethrow.
              if (utility.Utility.fromScalaPlugin(e)) {
                logError("makeProjectValuesByItem()",e);
              }
              else throw e;
            } catch (IndexNotReadyException e) {
              // we entered a dumb mode while processing this name, simply try again
              done = false;
            } finally {
              // only release the lock if a write action is trying to start
              if (checkClearWriteActionWaiting()) {
                lock.release();
                // yield to other threads to start the write action
                try {
                  sleep(0);
                } catch (InterruptedException e) {
                  throw new ProcessCanceledException();
                }
              }
            }

            if (updateFuture.isCancelled())
              throw new ProcessCanceledException();
          }
        }

        // don't need synchronized, access functions should copy pointer before working on it
        return result;

      } finally {
        // make sure the lock is released
        lock.release();
      }
    } finally {
      popScope();
    }
  }

  // get a combined environment at the given place
  public Environment.Env getLocalEnvironment(@NotNull PsiElement place, final int lastEdit) {

    pushScope("get local environment");
    try {
      final Map<PsiElement, Items.Item> items = new HashMap<PsiElement, Items.Item>();
      final Converter converter = new Converter(project, items);

      // we could cache the result of addBase, but it doesn't seem like it's worth it.
      addBase(converter);

      // ep will fill scopeItems (and it has its own store for special non-psi items and constructors)
      final EnvironmentProcessor ep = new EnvironmentProcessor(converter, place, lastEdit);

      pushScope("make ValueByItemQuery");
      final ValueByItemQuery vbi;
      try {
        vbi = new ValueByItemQuery() {
          final Map<Items.TypeItem, Items.Value[]> cache = new HashMap<Items.TypeItem, Items.Value[]>();
          final PsiShortNamesCache psicache = PsiShortNamesCache.getInstance(project);
          final GlobalSearchScope scope = ProjectScope.getContentScope(project);
          final IdFilter filter = new IdFilter() { @Override public boolean containsFileId(int id) { return true; } };
          final Set<Items.Value> result = new HashSet<Items.Value>();
          final EddyThread thread = EddyThread.getEddyThread();
          final Map<Items.TypeItem, Items.Value[]> vByItem = JavaItems.valuesByItem(ep.scopeItems);

          final Processor<PsiField> proc = new Processor<PsiField>() {
            @Override
            public boolean process(PsiField psiField) {
              // convert
              // check if type is really the type we want
              if (thread != null && thread.canceled())
                return false;
              result.add(converter.addField(psiField));
              return true;
            }
          };

          @Override
          public Items.Value[] query(Items.TypeItem type) {
            if (cache.containsKey(type))
              return cache.get(type);

            result.clear();

            // look up names in project
            String ts = type.qualified();

            // look up in pByItem (may be modified any time; we could make a copy of get(ts), but the background updater is not high priority
            // and it's ok if that waits for us)
            synchronized (byItemLock) {
              if (pByItem != null && pByItem.containsKey(ts)) {
                if (thread != null) thread.pushSoftInterrupts();
                for (String name : pByItem.get(ts)) {
                  psicache.processFieldsWithName(name, proc, scope, filter);
                }
                if (thread != null) thread.popSoftInterrupts();
              }
            }

            // filter by real type
            int count = 0;
            Iterator<Items.Value> it = result.iterator();
            while (it.hasNext()) {
              Items.Value v = it.next();
              if (v.item() != type) {
                it.remove();
              }
            }

            // look up in extraEnv
            Items.Value[] extras = Base.extraByItem().get(type);
            if (extras != null)
              Collections.addAll(result, extras);

            // look up in vByItem
            Items.Value[] varr = vByItem.get(type);
            if (varr != null)
              Collections.addAll(result, varr);

            Items.Value[] rarr = new Items.Value[result.size()];
            rarr = result.toArray(rarr);

            cache.put(type, rarr);
            return rarr;
          }
        };
      } finally {
        popScope();
      }

      // make a local trie of everything in scope which would not be found by the global lookup
      pushScope("make local trie");
      final Tries.Trie<Items.Item> localTrie;
      try {
        List<Items.Item> locals = new ArrayList<Items.Item>();

        for (Items.Item item : ep.localItems) {
          if (item instanceof Items.Local ||
            item instanceof Types.TypeVar ||
            item instanceof Items.ThisOrSuper) {
            //log("added local " + item);
            locals.add(item);
          }
        }
        Collections.addAll(locals, Base.extraItems());
        localTrie = Tarski.makeTrie(locals);
      } finally {
        popScope();
      }

      // copy the nameTrie structure pointer, may be overwritten any time
      final int[] structure = nameTrie;
      // make trie for global/project name lookup
      final Tries.LazyTrie<Items.Item> trie = new Tries.LazyTrie<Items.Item>(structure, new ItemGenerator(project, ProjectScope.getAllScope(project), converter, false));

      log("environment with " + ep.scopeItems.size() + " scope items taken at " + ep.placeInfo);
      return Tarski.environment(trie, localTrie, vbi, ep.scopeItems, ep.placeInfo);
    } finally {
      popScope();
    }
  }


}
