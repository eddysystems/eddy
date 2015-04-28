/* JavaEnvironment: Information about the project-wide environment
 *
 * The JavaEnvironment is the "large" part of the environment, precomputed every once
 * in a while by scanning the entire project.  While eddy is running, and between
 * recomputations, it is immutable.  Since storing PsiElements such as PsiClass for
 * long periods is verboten, JavaEnvironment's data structures consist entirely of
 * strings; these strings are converted into Items by ByItem and ItemGenerator.
 */

package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.java.LanguageLevel;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.SmartList;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import scala.Tuple2;
import tarski.*;
import tarski.Items.*;

import java.awt.EventQueue;
import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static com.eddysystems.eddy.engine.ChangeTracker.Snapshot;
import static com.eddysystems.eddy.engine.Utility.*;
import static utility.JavaUtils.*;
import static tarski.Flags.nullaryMethods;

// a class storing information about the environment.
public class JavaEnvironment {

  public static class NoJDKError extends RuntimeException {
    NoJDKError(String s) {
      super("No JDK found: " + s);
    }
  }

  @NotNull final Project project;
  @NotNull final ChangeTracker<String> nameTracker;
  @NotNull final ChangeTracker<TypeNameItemNamePair> fieldTracker, methodTracker;

  // scope and id filter for byItem lookups
  @NotNull final GlobalSearchScope byItemScope;
  @NotNull final IdFilter byItemFilter;

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

  // map types to fields and methods of that type.
  private Map<String, Set<String>> pByItemFields = null;
  private Map<String, Set<String>> pByItemMethods = null;

  // map package short names (e.g. math) to a list of all qualified names (java.math, com.google.common.math)
  private PackageIndex packageIndex = null;

  // Information about imports in our project
  private ImportTrie imports = null;

  // the background update thread
  private Future<?> updateFuture = null;
  private boolean needUpdate = false;

  public JavaEnvironment(@NotNull Project project,
                         @NotNull ChangeTracker<String> nameTracker,
                         @NotNull ChangeTracker<TypeNameItemNamePair> fieldTracker,
                         @NotNull ChangeTracker<TypeNameItemNamePair> methodTracker) {
    this.project = project;
    this.nameTracker = nameTracker;
    this.fieldTracker = fieldTracker;
    this.methodTracker = methodTracker;

    final boolean extremelySlow = false; // TODO: Would be really nice if searching everything wasn't extremely slow
    byItemScope = extremelySlow ? ProjectScope.getAllScope(project)
                                : ProjectScope.getProjectScope(project);
    byItemFilter = IdFilter.getProjectIdFilter(project, true);
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
      @Override public void run() {
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
          @Override public RuntimeException compute() {
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

      if (indicator != null)
        indicator.setText2("computing field names");
      final Snapshot[] nameSnap = new Snapshot[1];
      final Snapshot[] fieldSnap = new Snapshot[1];
      final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
      final String[] fieldNames = DumbService.getInstance(project).runReadActionInSmartMode(new Computable<String[]>() {
        @Override public String[] compute() {
          nameSnap[0] = nameTracker.snapshot();
          fieldSnap[0] = fieldTracker.snapshot();
          return cache.getAllFieldNames();
        }
      });

      if (updateFuture.isCancelled())
        return;

      if (indicator != null)
        indicator.setText2("computing method names");
      final Snapshot[] methodSnap = new Snapshot[1];
      final String[] methodNames = DumbService.getInstance(project).runReadActionInSmartMode(new Computable<String[]>() {
        @Override public String[] compute() {
          methodSnap[0] = methodTracker.snapshot();
          return cache.getAllMethodNames();
        }
      });

      if (indicator != null)
        indicator.setText2("building trie");
      nameTrie = prepareNameTrie(fieldNames, methodNames, packageIndex.getNames());
      nameTracker.forget(nameSnap[0]);

      // we're initialized starting here, we can live without byItem for a while
      _initialized = true;

      if (indicator != null)
        indicator.setText2("scanning imports");
      imports = scanImports();

      if (indicator != null) {
        indicator.setText2("computing type map");
        indicator.setIndeterminate(false);
      }

      pushScope("make project fields by item");
      try {
        final ByItemFieldProc fieldProc = new ByItemFieldProc();
        readLockLoop(fieldNames,indicator,new Processor<String>() {
          public boolean process(final String name) {
            return safeProcessFieldsWithName(cache, name, fieldProc, byItemScope, byItemFilter);
          }
        });
        pByItemFields = fieldProc.result;
        fieldTracker.forget(fieldSnap[0]);
      } finally { popScope(); }

      if (nullaryMethods) {
        pushScope("make project methods by item");
        try {
          final ByItemMethodProc methodProc = new ByItemMethodProc();
          readLockLoop(methodNames, indicator, new Processor<String>() {
            public boolean process(final String name) {
              return safeProcessMethodsWithName(cache, name, methodProc, byItemScope, byItemFilter);
            }
          });
          pByItemMethods = methodProc.result;
          methodTracker.forget(methodSnap[0]);
        } finally { popScope(); }
      }
    } finally {
      popScope();
      if (indicator != null)
        indicator.setIndeterminate(true);
    }

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
      List<PsiElement> psi = new ArrayList<PsiElement>();
      final EddyThread thread = EddyThread.getEddyThread();
      final JavaPsiFacade facade = JavaPsiFacade.getInstance(converter.project);
      for (final Item item : tarski.Base.baseItems()) {
        if (extra.contains(item))
          continue;
        final String name = item.qualified();
        psi.clear();
        if (thread != null) thread.pushSoftInterrupts();
        try {
          if (item instanceof Items.Package) {
            psi.add(facade.findPackage(name));
          } else if (item instanceof Items.ClassItem) {
            Collections.addAll(psi, facade.findClasses(name, scope));
          } else if (item instanceof Items.MethodItem) {
            String cname = ((Items.MethodItem) item).parent().qualified();
            for (PsiClass cls : facade.findClasses(cname, scope)) {
              if (cls != null) {
                PsiMethod[] methods = cls.findMethodsByName(item.name(), false);
                if (methods.length == 1)
                  psi.add(methods[0]);
                else
                  throw new NotImplementedError("overloaded method in base: " + item.qualified());
              }
            }
          } else
            throw new NotImplementedError("Unknown base type " + item.getClass());
          if (psi.isEmpty())
            throw new NoJDKError("Couldn't find " + name);
        } finally {
          if (thread != null) thread.popSoftInterrupts();
        }

        // add all PsiElements we found (these are equivalent PsiElements from different JDKs or library versions)
        for (PsiElement p : psi)
          if (!converter.knows(p))
            converter.put(p, item);
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

  private int[] prepareNameTrie(final String[] fieldNames, final String[] methodNames, final String[] packageNames) {
    pushScope("prepare lazy trie");
    try {
      if (updateFuture.isCancelled())
        return null;

      final String[] classNames = DumbService.getInstance(project).runReadActionInSmartMode(new Computable<String[]>() {
        @Override
        public String[] compute() {
          return PsiShortNamesCache.getInstance(project).getAllClassNames();
        }
      });

      if (updateFuture.isCancelled())
        return null;

      final String[] allNames = concatenate(classNames,fieldNames,methodNames,packageNames);

      if (updateFuture.isCancelled())
        return null;

      // there may be duplicates, but we don't particularly care
      Arrays.sort(allNames);
      return JavaTrie.makeTrieStructure(allNames);
    } finally { popScope(); }
  }

  // TODO: this should use the same pause mechanism as EddyThread (counted)
  public static Pause pause = new Pause();

  private static abstract class ByItemProc<A extends PsiMember> implements Processor<A> {
    final Map<String,Set<String>> result = new HashMap<String,Set<String>>();
    private final Stack<PsiType> work = new Stack<PsiType>();
    private final Set<PsiType> seen = new HashSet<PsiType>();

    // Put this name into the string map for its type and all its supertypes
    void absorb(final String name, final PsiType type) {
      seen.clear();
      work.clear();
      work.push(type);
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
        final String typeName = t.getCanonicalText();
        Set<String> values = result.get(typeName);
        if (values == null) {
          values = new HashSet<String>();
          result.put(typeName,values);
        }
        values.add(name);

        for (final PsiType s : t.getSuperTypes()) {
          if (!seen.contains(s)) {
            seen.add(s);
            work.push(s);
          }
        }
      }
    }
  }
  private final class ByItemFieldProc extends ByItemProc<PsiField> {
    public boolean process(final PsiField f) {
      if (updateFuture.isCancelled())
        return false;
      try {
        // Restrict to only public fields, those in scope (protected or private) are added to vByItem
        if (f.hasModifierProperty(PsiModifier.PUBLIC))
          absorb(f.getName(),f.getType());
      } catch (AssertionError e) {
        // If we're in the Scala plugin, log and squash the error.  Otherwise, rethrow.
        if (utility.Utility.fromScalaPlugin(e)) logError("makeProjectValuesByItem()",e);
        else throw e;
      }
      return true;
    }
  }
  private final class ByItemMethodProc extends ByItemProc<PsiMethod> {
    public boolean process(final PsiMethod m) {
      if (updateFuture.isCancelled())
        return false;
      try {
        // Restrict to only public methods, those in scope (protected or private) are added to vByItem
        if (m.hasModifierProperty(PsiModifier.PUBLIC)) {
          final PsiType type = m.getReturnType();
          if (ByItem.considerMethod(m,type))
            absorb(m.getName(),type);
        }
      } catch (AssertionError e) {
        // If we're in the Scala plugin, log and squash the error.  Otherwise, rethrow.
        if (utility.Utility.fromScalaPlugin(e)) logError("makeProjectValuesByItem()",e);
        else throw e;
      }
      return true;
    }
  }

  // Do something for every element of a list, with a bunch of complex logic for handling temporary failure
  private <A> boolean readLockLoop(final A[] values, final @Nullable ProgressIndicator indicator, final Processor<A> proc) {
    final Utility.SmartReadLock lock = new Utility.SmartReadLock(project);
    final double n = values.length;
    int i = 0;
    try {
      for (final A s : values) {
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
            if (!proc.process(s))
              return false;
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
    } finally {
      // make sure the lock is released
      lock.release();
    }
    return true;
  }

  // get a combined environment at the given place
  public Environment.Env getLocalEnvironment(@NotNull PsiElement place, final int lastEdit) {

    pushScope("get local environment");
    try {
      final Converter converter = new Converter(project, new HashMap<PsiElement,Item>());

      // we could cache the result of addBase, but it doesn't seem like it's worth it.
      addBase(converter);
      
      // ep will fill scopeItems (and it has its own store for special non-psi items and constructors)
      final EnvironmentProcessor ep = new EnvironmentProcessor(converter, place, lastEdit);

      // stuff from trackers
      final List<TypeNameItemNamePair> newFieldPairs = fieldTracker.values();
      final List<TypeNameItemNamePair> newMethodPairs = methodTracker.values();
      final List<String> newNames = nameTracker.values();

      // schedule a background update if the trackers get too large
      // TODO: split requestUpdate into two
      if (newFieldPairs.size()+newMethodPairs.size() > rebuildByItemThreshold || newNames.size() > rebuildNamesThreshold) {
        log("requesting background update for " + newFieldPairs.size() + " fields, "
          + newMethodPairs.size() + " methods, " + newNames.size() + " names.");
        requestUpdate();
      }

      pushScope("make ValueByItemQuery");
      final ValueByItemQuery vbi;
      try {
        vbi = new ByItem(converter,pByItemFields,pByItemMethods,newFieldPairs,newMethodPairs,
                         ep.localItems,byItemScope,byItemFilter);
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

      // Grab any information we have about imports
      final ImportTrie imports = this.imports != null ? this.imports : tarski.Pr.defaultImports();

      // Find the language level for this file (don't go through strings, API changed)
      final LanguageLevel llevel = ((PsiJavaFile) place.getContainingFile()).getLanguageLevel();
      final int level =
        llevel.isAtLeast(LanguageLevel.JDK_1_8) ? Levels.getLevel(Levels$.MODULE$.Java8()) :
        llevel.isAtLeast(LanguageLevel.JDK_1_7) ? Levels.getLevel(Levels$.MODULE$.Java7()) :
        llevel.isAtLeast(LanguageLevel.JDK_1_6) ? Levels.getLevel(Levels$.MODULE$.Java6()) :
        llevel.isAtLeast(LanguageLevel.JDK_1_5) ? Levels.getLevel(Levels$.MODULE$.Java5()) :
        llevel.isAtLeast(LanguageLevel.JDK_1_4) ? Levels.getLevel(Levels$.MODULE$.Java1_4()) :
        llevel.isAtLeast(LanguageLevel.JDK_1_3) ? Levels.getLevel(Levels$.MODULE$.Java1_3()) :
        Levels.getLevel(Levels$.MODULE$.Unknown());

      // Add relevant extraItems to scopeItems
      for (final Item i : tarski.Base.extraItems())
        if (i instanceof LitValue || i instanceof LangTypeItem)
          ep.scopeItems.put(i, 1);

      log("environment with " + ep.scopeItems.size() + " scope items taken at " + ep.placeInfo);
      return Tarski.environment(bigTrie, smallTrie, vbi, imports, ep.scopeItems, ep.placeInfo, level);
    } finally { popScope(); }
  }

  ImportTrie scanImports() {
    // Grab list of Java files outside of a read action
    final List<VirtualFile> files = new SmartList<VirtualFile>();
    ProjectRootManager.getInstance(project).getFileIndex().iterateContent(new ContentIterator() {
      public boolean processFile(final VirtualFile file) {
        if (file.getFileType() == StdFileTypes.JAVA)
          files.add(file);
        return true;
      }
    });

    // Process each file inside a separate read action
    final DumbService dumb = DumbService.getInstance(project);
    final PsiManager manager = PsiManager.getInstance(project);
    final ImportTrie imports = new ImportTrie();
    for (final VirtualFile file : files)
      dumb.runReadActionInSmartMode(new Runnable() { @Override public void run() {
        if (!file.isValid()) return;
        final PsiFile java = manager.findFile(file);
        if (!(java instanceof PsiJavaFile)) return;
        final PsiImportList imps = ((PsiJavaFile) java).getImportList();
        if (imps == null) return;
        for (final PsiImportStatementBase imp : imps.getAllImportStatements()) {
          final PsiJavaCodeReferenceElement ref = imp.getImportReference();
          if (ref != null) {
            final String qual = ref.getQualifiedName();
            if (qual != null)
              imports.add(qual);
          }
        }
      }});

    // Nearly done
    imports.addDefaults();
    return imports;
  }
}
