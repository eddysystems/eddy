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
import com.intellij.util.SmartList;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import tarski.*;
import tarski.Items.BaseItem;
import tarski.Items.ClassItem;
import tarski.Items.ConstructorItem;
import tarski.Items.Item;

import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.locks.Lock;

import static com.eddysystems.eddy.engine.ChangeTracker.Snapshot;
import static com.eddysystems.eddy.engine.Utility.log;
import static com.eddysystems.eddy.engine.Utility.logError;
import static utility.JavaUtils.*;

// a class storing information about the environment.
public class JavaEnvironment {

  public static class NoJDKError extends RuntimeException {
    NoJDKError(String s) {
      super("No JDK found: " + s);
    }
  }

  @NotNull final Project project;
  @NotNull final ChangeTracker<String> nameTracker;
  @NotNull final ChangeTracker<TypeNameItemNamePair> valueTracker;

  // scope and id filter for byItem lookups
  @NotNull final GlobalSearchScope scope;
  @NotNull final IdFilter filter;

  final int rebuildByItemThreshold = 100;
  final int rebuildNamesThreshold = 100;

  private boolean _initialized = false;
  public boolean initialized() {
    return _initialized;
  }

  // pre-computed data structures to enable fast creation of appropriate scala Env instances

  // these may be overwritten any time by new objects created in an updating process, but they are never changed.
  // Grabbing a copy and working with it is always safe even without a lock

  // a trie encapsulating the global list of all names. This is never edited, so it's safe to use without a lock
  private int[] nameTrie = null;

  // map types to items of that type.
  private Map<String, Set<String>> pByItem = null;

  // map package short names (e.g. math) to a list of all qualified names (java.math, com.google.common.math)
  private PackageIndex packageIndex = null;

  // the background update thread
  private Future<?> updateFuture = null;
  private boolean needUpdate = false;

  public JavaEnvironment(@NotNull Project project, @NotNull ChangeTracker<String> nameTracker, @NotNull ChangeTracker<TypeNameItemNamePair> valueTracker) {
    this.project = project;
    this.nameTracker = nameTracker;
    this.valueTracker = valueTracker;

    scope = ProjectScope.getProjectScope(project);
    filter = IdFilter.getProjectIdFilter(project, true);
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

  // for initialization -- return whether initialization was successful
  public synchronized String updateSync(@Nullable final ProgressIndicator indicator) {
    requestUpdate(indicator);
    try {
      updateFuture.get();
    } catch (ExecutionException e) {
      return e.getMessage();
    } catch (InterruptedException e) {
      return e.getMessage();
    }
    return _initialized ? "" : "No JDK found.";
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
        // we'll spend some time especially at startup, so let people know it's not our fault
        if (indicator != null)
          indicator.setText2("waiting for index to be ready");

        // don't throw inside there -- Catch and rethrow to avoid logging
        RuntimeException error = DumbService.getInstance(project).runReadActionInSmartMode(new Computable<RuntimeException>() {
          @Override
          public RuntimeException compute() {
            try {
              if (indicator != null)
                indicator.setText2("adding base environment");

              addBase(new Converter(project, new HashMap<PsiElement, Item>()));
              return null;
            } catch (NoJDKError e) {
              return e;
            }
          }
        });

        if (error != null) {
          log("No JDK found, aborting initialization.");
          return;
        }
      }

      if (indicator != null)
        indicator.setText2("building package index");

      pushScope("build package index");
      try {
        // TODO: needs to be updated on file system changes, our current update requests are not the correct ones for this
        packageIndex = new PackageIndex(project);
      } finally { popScope(); }

      final Snapshot[] nameSnap = new Snapshot[1];
      final Snapshot[] valueSnap = new Snapshot[1];

      if (indicator != null)
        indicator.setText2("computing field names");

      final String[] fieldNames = DumbService.getInstance(project).runReadActionInSmartMode( new Computable<String[]>() { @Override public String[] compute() {
        nameSnap[0] = nameTracker.snapshot();
        valueSnap[0] = valueTracker.snapshot();
        return PsiShortNamesCache.getInstance(project).getAllFieldNames();
      }});

      if (indicator != null)
        indicator.setText2("building trie");

      nameTrie = prepareNameTrie(fieldNames);

      pushScope("forget names");
      try {
        nameTracker.forget(nameSnap[0]);
      } finally { popScope(); }

      // we're initialized starting here, we can live without the makeProjectValuesByItem for a while
      _initialized = true;

      if (indicator != null) {
        indicator.setText2("computing type map");
        indicator.setIndeterminate(false);
      }

      try {
        pByItem = makeProjectValuesByItem(fieldNames, indicator);
        pushScope("forget values");
        try {
          valueTracker.forget(valueSnap[0]);
        } finally { popScope(); }
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
      final Set<Item> extra = new HashSet<Item>();
      Collections.addAll(extra, Base.extraItems());

      // Add classes and packages
      final EddyThread thread = EddyThread.getEddyThread();
      final JavaPsiFacade facade = JavaPsiFacade.getInstance(converter.project);
      for (final Item item : tarski.Base.baseItems()) {
        if (extra.contains(item))
          continue;
        final String name = item.qualified();
        PsiElement psi;
        if (item instanceof Items.Package) {
          if (thread != null) thread.pushSoftInterrupts();
          psi = facade.findPackage(name);
          if (thread != null) thread.popSoftInterrupts();
        } else if (item instanceof Items.ClassItem) {
          if (thread != null) thread.pushSoftInterrupts();
          psi = facade.findClass(name, scope);
          if (thread != null) thread.popSoftInterrupts();
        } else if (item instanceof Items.MethodItem) {
          if (thread != null) thread.pushSoftInterrupts();
          String cname = ((Items.MethodItem) item).parent().qualified();
          PsiClass cls = facade.findClass(cname, scope);
          psi = null;
          if (cls != null) {
            PsiMethod[] methods = cls.findMethodsByName(item.name(), false);
            if (methods.length == 1)
              psi = methods[0];
          }
          if (thread != null) thread.popSoftInterrupts();
        } else
          throw new NotImplementedError("Unknown base type " + item.getClass());
        if (psi == null)
          throw new NoJDKError("Couldn't find " + name);

        if (!converter.knows(psi))
          converter.put(psi, item);
      }

      // Add class members
      for (final Item item : tarski.Base.baseItems()) {
        if (extra.contains(item) || !(item instanceof Items.ClassItem))
          continue;
        final ClassItem item_ = (ClassItem) item;
        final String name = item.qualified();
        final boolean isClass = item_.isClass();
        assert !isClass || item instanceof BaseItem; // Required for constructors to work
        if (thread != null) thread.pushSoftInterrupts();
        final List<ConstructorItem> cons = converter.addClassMembers(facade.findClass(name, scope), item_, isClass);
        if (thread != null) thread.popSoftInterrupts();
        if (isClass)
          ((BaseItem)item).constructors_$eq(cons.toArray(new ConstructorItem[cons.size()]));
      }
    } finally { popScope(); }
  }

  private int[] prepareNameTrie(String[] fieldNames) {
    pushScope("prepare lazy trie");
    try {
      if (updateFuture.isCancelled())
        return null;

      final String[] classNames = DumbService.getInstance(project).runReadActionInSmartMode( new Computable<String[]>() { @Override public String[] compute() {
        return PsiShortNamesCache.getInstance(project).getAllClassNames();
      }});

      if (updateFuture.isCancelled())
        return null;

      final String[] methodNames = DumbService.getInstance(project).runReadActionInSmartMode( new Computable<String[]>() { @Override public String[] compute() {
        return PsiShortNamesCache.getInstance(project).getAllMethodNames();
      }});

      if (updateFuture.isCancelled())
        return null;

      final String[] allNames = concatenate(classNames,fieldNames,methodNames);

      if (updateFuture.isCancelled())
        return null;

      // there may be duplicates, but we don't particularly care
      Arrays.sort(allNames);
      return JavaTrie.makeTrieStructure(allNames);
    } finally { popScope(); }
  }

  // TODO: this should use the same pause mechanism as EddyThread (counted)
  public static Pause pause = new Pause();

  private Map<String, Set<String>> makeProjectValuesByItem(String[] fieldNames, @Nullable ProgressIndicator indicator) {
    pushScope("make project values by item");

    // Prepare to grab index write locks to avoid crazy deadlocks.  This is a terrible hack around broken OPC.
    final Lock fqnLock = ((StubIndexImpl)StubIndex.getInstance())
      .getWriteLock(JavaFullClassNameIndex.getInstance().getKey());

    try {
      final Map<String,Set<String>> result = new HashMap<String,Set<String>>();

      final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
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
            // restrict this to only public fields, FieldItems in scope (protected or private) are added to vByItem
            if (!f.hasModifierProperty(PsiModifier.PUBLIC))
              return true;

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
              // we entered a dumb mode while processing this name, try again
              done = false;
            } finally {
              // only release the lock if a write action is trying to start
              if (pause.paused()) {
                lock.release();
                // yield to other threads to start the write action
                try {
                  pause.waitForEnd();
                } catch (ThreadDeath e) {
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
    } finally { popScope(); }
  }

  // get a combined environment at the given place
  public Environment.Env getLocalEnvironment(@NotNull PsiElement place, final int lastEdit) {

    pushScope("get local environment");
    try {
      final Map<PsiElement,Item> items = new HashMap<PsiElement,Item>();
      final Converter converter = new Converter(project, items);

      // we could cache the result of addBase, but it doesn't seem like it's worth it.
      addBase(converter);
      
      // ep will fill scopeItems (and it has its own store for special non-psi items and constructors)
      final EnvironmentProcessor ep = new EnvironmentProcessor(converter, place, lastEdit);

      // stuff from trackers
      final List<TypeNameItemNamePair> newValuePairs = valueTracker.values();
      final List<String> newNames = nameTracker.values();

      // schedule a background update if the trackers get too large
      // TODO: split requestUpdate into two
      if (newValuePairs.size() > rebuildByItemThreshold || newNames.size() > rebuildNamesThreshold) {
        log("requesting background update for " + newValuePairs.size() + " values, " + newNames.size() + " names.");
        requestUpdate();
      }

      pushScope("make ValueByItemQuery");
      final ValueByItemQuery vbi;
      try {
        vbi = new ByItem(converter,pByItem,newValuePairs,ep.localItems);
      } finally { popScope(); }

      // Make trie for global/project name lookup
      final int[] bigStructure = nameTrie;
      final JavaTrie.Generator<Item> bigGenerator = new ItemGenerator(project, ProjectScope.getAllScope(project), converter, packageIndex);
      final Tries.Queriable<Item> bigTrie = new Tries.LazyTrie<Item>(bigStructure, bigGenerator);

      // Make a small trie with both locals and recently added names
      pushScope("make small trie");
      final Tries.Queriable<Item> smallTrie;
      try {
        final List<String> smallNames = new ArrayList<String>();

        // Grab recent names from the change tracker.  Remove any that also occur in the big trie
        final Set<String> recentSet = new HashSet<String>();
        for (final String name : newNames)
          if (JavaTrie.exactNode(bigStructure,name.toCharArray()) < 0) {
            smallNames.add(name);
            recentSet.add(name);
          }

        // Collect locals and extras
        final List<Item> localsAndExtras = new ArrayList<Item>();
        Collections.addAll(localsAndExtras, Base.extraItems());
        for (final Item item : ep.localItems) {
          if (item instanceof Items.Local ||
              item instanceof Types.TypeVar ||
              item instanceof Items.ThisOrSuper) {
            //log("added local " + item);
            localsAndExtras.add(item);
          }
        }

        // Turn locals and extras into a map
        final Map<String,List<Item>> itemMap = new HashMap<String,List<Item>>();
        for (final Item item : localsAndExtras) {
          final String name = item.name();
          List<Item> list = itemMap.get(name);
          if (list == null) {
            list = new SmartList<Item>();
            itemMap.put(name,list);
            smallNames.add(name);
          }
          list.add(item);
        }

        // Make a generator combining itemMap, recentSet, and the bigGenerator from above
        final JavaTrie.Generator<Item> smallGenerator = new JavaTrie.Generator<Item>() {
          public Item[] lookup(final String name) {
            final Item[] recent = recentSet.contains(name) ? bigGenerator.lookup(name) : null;
            final List<Item> items = itemMap.get(name);
            if (items == null)
              return recent;
            final int ni = items.size();
            final int nr = recent==null ? 0 : recent.length;
            final Item[] results = new Item[ni+nr];
            items.toArray(results);
            if (recent != null)
              System.arraycopy(recent,0,results,ni,nr);
            return results;
          }
        };

        // Build the small trie
        Collections.sort(smallNames);
        final int[] smallStructure = JavaTrie.makeTrieStructure(smallNames.toArray(new String[smallNames.size()]));
        smallTrie = new Tries.LazyTrie<Item>(smallStructure,smallGenerator);
      } finally { popScope(); }

      log("environment with " + ep.scopeItems.size() + " scope items taken at " + ep.placeInfo);
      return Tarski.environment(bigTrie, smallTrie, vbi, ep.scopeItems, ep.placeInfo);
    } finally { popScope(); }
  }


}
