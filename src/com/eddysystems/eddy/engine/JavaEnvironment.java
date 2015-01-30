package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.IndexNotReadyException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.*;
import com.intellij.psi.impl.java.stubs.index.JavaFullClassNameIndex;
import com.intellij.psi.impl.java.stubs.index.JavaShortClassNameIndex;
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

  static class PsiGenerator implements Tries.Generator<Items.Item> {

    static final int cacheSize = 10000;
    final LRUCache<String, Items.Item[]> cache = new LRUCache<String, Items.Item[]>(cacheSize);

    final Project project;
    final GlobalSearchScope scope;
    final PsiShortNamesCache psicache;
    final IdFilter filter = new IdFilter() { @Override public boolean containsFileId(int id) { return true; } };
    final Converter converter;

    // true if there's a chance that element is visible from outside its file. Only elements that are private or
    // inside private or anonymous elements or that are local are not potentially visible.
    boolean possiblyVisible(PsiModifierListOwner element) {
      PsiElement container = null;
      try {
        container = Place.containing(element, project);
      } catch (Place.UnexpectedContainerError e) {
        log(e.getMessage());
        return false;
      }

      // anything toplevel in a package is at most protected
      if (container instanceof PsiPackage) {
        return true;
      }

      // anything private is out
      if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
        return false;
      }

      // everything else, depends on the container
      if (container instanceof PsiModifierListOwner) {
        return possiblyVisible((PsiModifierListOwner)container);
      } else
        return false;
    }

    PsiGenerator(Project project, GlobalSearchScope scope, Converter conv) {
      this.project = project;
      this.scope = scope;
      psicache = PsiShortNamesCache.getInstance(project);
      converter = conv;
    }

    private Items.Item[] generate(String s) {
      final EddyThread thread = EddyThread.getEddyThread();
      final List<Items.Item> results = new ArrayList<Items.Item>();

      final Processor<PsiClass> classProc = new Processor<PsiClass>() {
      @Override
      public boolean process(PsiClass cls) {
        if (thread != null && thread.canceled())
          return false;
        if (possiblyVisible(cls))
          results.add(converter.addClass(cls, false));
        return true;
      }
      };

      final Processor<PsiMethod> methodProc = new Processor<PsiMethod>() {
      @Override
      public boolean process(PsiMethod method) {
        if (thread != null && thread.canceled())
          return false;
        if (possiblyVisible(method) && !Converter.isConstructor(method))
          results.add(converter.addMethod(method));
        return true;
      }
      };

      final Processor<PsiField> fieldProc = new Processor<PsiField>() {
      @Override
      public boolean process(PsiField fld) {
        if (thread != null && thread.canceled())
          return false;
        if (possiblyVisible(fld))
          results.add(converter.addField(fld));
        return true;
      }
      };

      if (thread != null)
        thread.setSoftInterrupts(true);
      psicache.processClassesWithName(s, classProc, scope, filter);
      psicache.processMethodsWithName(s, methodProc, scope, filter);
      psicache.processFieldsWithName(s, fieldProc, scope, filter);
      if (thread != null)
        thread.setSoftInterrupts(false);
      return results.toArray(new Items.Item[results.size()]);
    }

    @Override @NotNull
    public Items.Item[] lookup(String s) {
      Items.Item[] result = cache.get(s);

      if (result != null)
        return result;
      else
        result = generate(s);

      // add to cache
      cache.put(s, result);
      return result;
    }
  }

  // rebuild the local environment if rebuildRatio of localEnv is deleted, or rebuildRatio of denv is in addedItems
  // (but require at least rebuildMinChanges additions or deletions)
  final static int rebuildMinChanges = 20;
  final static float rebuildRatio = .1f;

  @NotNull final Project project;
  @NotNull final Converter converter;

  private boolean _initialized = false;

  public boolean initialized() {
    return _initialized;
  }

  // all these are possibly accessed by several threads. Modifying or non-atomic read access requiring consistency to any
  // of items, localItems, addedItems has to be synchronized(this). If the caller is inside an IntelliJ write action
  // (such as anything called from the PsiListener), synchronization is not necessary because all other accessing threads
  // are inside IntelliJ read actions, and will not run during a write action)

  // global (library) environment. Only added to, never deleted from or changed. All public items not in project files go in here
  Map<PsiElement, Items.Item> items = new HashMap<PsiElement, Items.Item>();

  // mutable, local (project) environment, can be added to and changed by edits. All public items in project files go in here
  Map<PsiElement, Items.Item> localItems = new HashMap<PsiElement, Items.Item>();

  // items added since the last time the local environment was built (to be added to the scope environment)
  Map<PsiElement, Items.Item> addedItems = new HashMap<PsiElement, Items.Item>();
  int nDeletions;

  // items found in scope by the EnvironmentProcessor
  // TODO: replace this with an LRUCache or at least, randomly cut it down once in a while. Currently, this is never cleaned out
  Map<PsiElement, Items.Item> scopeItems = new HashMap<PsiElement, Items.Item>();

  // pre-computed data structures to enable fast creation of appropriate scala Env instances

  // when a local env is created, it contains: A global lookup object (created from the global list of all names and a generator object
  // able to translate a name into a list of items), a trie storing Local items, a byItem map for local items, and a trie and byItem map
  // containing items added to locals (but not in the trie yet), and the locals found by the EnvironmentProcessor.
  Tries.LazyTrie<Items.Item> sTrie = null; // can't be final because initialized in initialize, but never changed after
  Tries.DTrie<Items.Item> dTrie = null;
  Map<Items.TypeItem, Items.Value[]> dByItem = null;

  // dynamic by item needs to be rebuilt a lot more than the trie.
  boolean byItemNeedsRebuild = false;

  public JavaEnvironment(@NotNull Project project) {
    this.project = project;
    converter = new Converter(this, items, localItems, addedItems);
  }

  public void initialize(@Nullable ProgressIndicator indicator) {
    pushScope("make base environment");
    try {
      // add base items
      DumbService.getInstance(project).runReadActionInSmartMode(new Runnable() {
        @Override
        public void run() {
          addBase();
        }
      });

      // get all class names
      final List<String> classNames = new ArrayList<String>();
      DumbService.getInstance(project).runReadActionInSmartMode(new Runnable() {
        @Override
        public void run() {
          Collections.addAll(classNames, PsiShortNamesCache.getInstance(project).getAllClassNames());
        }
      });

      if (indicator != null)
        indicator.setIndeterminate(false);

      // takes care of read action business inside (this is where all the work happens)
      storeProjectClassInfo(classNames, indicator);

      if (indicator != null)
        indicator.setIndeterminate(true);

      DumbService.getInstance(project).runReadActionInSmartMode(new Runnable() { @Override public void run() {
        buildDynamicEnv();
      }});

      // make global item generator
      sTrie = DumbService.getInstance(project).runReadActionInSmartMode(new Computable<Tries.LazyTrie<Items.Item>>() { @Override public Tries.LazyTrie<Items.Item> compute() {
        return prepareLazyTrie();
      }});
      _initialized = true;
    } finally { popScope(); }
  }

  synchronized boolean knows(PsiElement elem) {
    return items.containsKey(elem) || localItems.containsKey(elem) || scopeItems.containsKey(elem);
  }

  @Nullable
  synchronized Items.Item lookup(PsiElement elem, boolean checkLocal) {
    // Everything in addedItems is also in localItems.
    Items.Item i = items.get(elem);
    if (i == null)
      i = localItems.get(elem);
    if (i == null && checkLocal)
      i = scopeItems.get(elem);
    return i;
  }

  // find an element. If it was found in the local part of the environment, move it to the global part
  @Nullable
  synchronized Items.Item lookupAndMove (PsiElement e) {
    Items.Item it = lookup(e, false);
    if (it == null) {
      it = scopeItems.get(e);
      if (it != null) {
        converter.put(e,it);
        scopeItems.remove(e);
      }
    }
    return it;
  }

  private void addBase() {
    pushScope("add base");
    try {
      final GlobalSearchScope scope = ProjectScope.getAllScope(project);

      // Extra things don't correspond to PsiElements
      final Set<Items.Item> extra =  new HashSet<Items.Item>();
      Collections.addAll(extra, Base.extraEnv().allItems());

      // Add classes and packages
      final JavaPsiFacade facade = JavaPsiFacade.getInstance(project);
      for (Items.Item item : tarski.Base.baseEnv().allItems()) {
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
        converter.put(psi, item);
      }

      // Add constructors
      for (Items.Item item : tarski.Base.baseEnv().allItems()) {
        if (!(item instanceof Items.ConstructorItem))
          continue;
        final String clsName = ((Items.ConstructorItem)item).parent().qualified();
        final PsiClass cls = facade.findClass(clsName,scope);
        assert cls != null;
        final PsiMethod[] cons = cls.getConstructors();
        if (cons.length != 1)
          log("found " + cons.length + " constructors for object " + cls);
        converter.put(cons[0],item);
      }

      // Add class members
      for (Items.Item item : tarski.Base.baseEnv().allItems()) {
        if (extra.contains(item) || !(item instanceof Items.ClassItem))
          continue;
        final String name = item.qualified();
        converter.addClassMembers(facade.findClass(name, scope), (Items.ClassItem) item);
       }
    } finally { popScope(); }
  }

  private Tries.LazyTrie<Items.Item> prepareLazyTrie() {
    Tries.LazyTrie<Items.Item> t = null;

    pushScope("prepare lazy global trie");
    try {
      String[] classNames = PsiShortNamesCache.getInstance(project).getAllClassNames();
      String[] fieldNames = PsiShortNamesCache.getInstance(project).getAllFieldNames();
      String[] methodNames = PsiShortNamesCache.getInstance(project).getAllMethodNames();
      String[] allNames = new String[classNames.length + fieldNames.length + methodNames.length];

      System.arraycopy(classNames, 0, allNames, 0, classNames.length);
      System.arraycopy(fieldNames, 0, allNames, classNames.length, fieldNames.length);
      System.arraycopy(methodNames, 0, allNames, classNames.length+fieldNames.length, methodNames.length);

      // there may be duplicates, but we don't care
      Arrays.sort(allNames);

      t = new Tries.LazyTrie<Items.Item>(JavaTrie.makeTrieStructure(allNames), new PsiGenerator(project, ProjectScope.getLibrariesScope(project), converter));
    } finally { popScope(); }

    return t;
  }

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

  private void storeProjectClassInfo(List<String> classNames, @Nullable ProgressIndicator indicator) {
    pushScope("store project classes");

    // Prepare to grab index write locks to avoid crazy deadlocks.  This is a terrible hack around broken OPC.
    final Lock fqnLock = ((StubIndexImpl)StubIndex.getInstance())
      .getWriteLock(JavaFullClassNameIndex.getInstance().getKey());

    try {
      final GlobalSearchScope scope = ProjectScope.getContentScope(project);
      final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
      final IdFilter filter = IdFilter.getProjectIdFilter(project, false);
      final Processor<PsiClass> proc = new Processor<PsiClass>() {
        @Override
        public boolean process(final PsiClass cls) {
          fqnLock.lock();
          try {
            converter.addClass(cls, true);
          } catch (AssertionError e) {
            // If we're in the Scala plugin, log and squash the error.  Otherwise, rethrow.
            if (utility.Utility.fromScalaPlugin(e)) logError("storeProjectClassInfo()",e);
            else throw e;
          } finally {
            fqnLock.unlock();
          }
          return true;
        }
      };

      final Utility.SmartReadLock lock = new Utility.SmartReadLock(project);
      int i = 0;
      final double n = classNames.size();
      try {
        for (final String s : classNames) {
          if (indicator != null) {
            indicator.checkCanceled();
            indicator.setFraction(i++/n);
          }

          // pretend we're doing a big read action here, if we get stumped by a dump mode, repeat until it passes
          boolean done = false;
          while (!done) {
            done = true;
            lock.acquire();
            try {
              cache.processClassesWithName(s, proc, scope, filter);
            } catch (AssertionError e) {
              // If we're in the Scala plugin, log and squash the error. Don't retry. Otherwise, rethrow.
              if (utility.Utility.fromScalaPlugin(e)) {
                logError("storeProjectClassInfo()",e);
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
          }
        }
      } finally {
        // make sure the lock is released
        lock.release();
      }
    } finally {
      popScope();
    }
  }

  private static List<Items.Item> pruneItems(Collection<Items.Item> items) {
    List<Items.Item> pruned = new ArrayList<Items.Item>(items.size());
    for (final Items.Item i : items)
      if (i.name() != null && !(i instanceof Items.ConstructorItem))
        pruned.add(i);
    return pruned;
  }

  private void buildDynamicEnv() {
    pushScope("building project trie");
    try {
      final List<Items.Item> items = pruneItems(localItems.values());
      items.addAll(Arrays.asList(Base.extraEnv().allItems())); // Add int, false, null, etc.

      dTrie = Tarski.makeDTrie(items);
      dByItem = JavaItems.valuesByItem(dTrie.values());

      // clear volatile stores
      addedItems.clear();
      nDeletions = 0;
    } finally { popScope(); }
  }

  private boolean localEnvNeedsRebuild() {
    int changes = Math.max(nDeletions, addedItems.size());
    if (changes < rebuildMinChanges)
      return false;
    else
      return (float)changes / dTrie.values().length > rebuildRatio;
  }

  // get a combined environment at the given place
  Environment.Env getLocalEnvironment(@NotNull PsiElement place, final int lastEdit) {
    synchronized(this) {
      if (localEnvNeedsRebuild())
        buildDynamicEnv();
      else if (byItemNeedsRebuild)
        dByItem = JavaItems.valuesByItem(dTrie.values());
    }

    // ep will fill scopeItems (and it has its own store for special non-psi items and constructors)
    EnvironmentProcessor ep = new EnvironmentProcessor(project, this, scopeItems, place, lastEdit);

    final List<Items.Item> pruned = pruneItems(ep.localItems);

    Tries.DTrie<Items.Item> dt;
    Map<Items.TypeItem, Items.Value[]> dbi;

    // need sync on this to make addedItems.values() safe and in sync with dTrie
    synchronized (this) {
      // addedItems never contains constructors (see Converter.put() and changeItemName())
      pruned.addAll(addedItems.values());
      dt = dTrie;
      dbi = dByItem;
    }

    final Items.Item[] newArray = pruned.toArray(new Items.Item[pruned.size()]);
    return Tarski.environment(sTrie, dt, Tarski.makeTrie(newArray), dbi, JavaItems.valuesByItem(newArray), ep.scopeItems, ep.placeInfo);
  }

  synchronized void addItem(PsiElement elem) {
    // the handler calls this for each interesting element that is added, there is no need for Psi tree
    // traversal in here

    // add to localItems and addedItems, make sure it's not known already
    assert !knows(elem);

    // this adds to localItems and possibly addedItems
    Items.Item it = converter.addItem(elem);

    // save a copy in addedItems (constructors don't go there)
    if (it instanceof Items.ConstructorItem) {
      ((Items.CachedConstructorsItem)((Items.ConstructorItem) it).parent()).invalidateConstructors();
    }

    // if we added a class, check if we can fill in some of the undefined references
    if (it instanceof Items.RefTypeItem) {
      // go through all (local) items and check whether we can fill in some undefined references
      // this may add inheritance structure that invalidates valuesByItem
      for (Items.Item i : localItems.values())
        if (i instanceof Converter.ReferencingItem)
          byItemNeedsRebuild = byItemNeedsRebuild || ((Converter.ReferencingItem)i).fillUnresolvedReferences();
    }
  }

  synchronized void deleteItem(PsiElement elem) {

    // this is called from beforeDelete, so elem is still valid

    // the handler calls this for each interesting element that will be deleted, there is no need for Psi tree
    // traversal in here

    // any elements in the psi tree below this element have been deleted already, if they needed to be deleted

    Items.Item it = lookup(elem, false);

    // do we even care?
    if (it == null)
      return;

    log("deleting " + it);
    it.delete();

    localItems.remove(elem);
    if (addedItems.containsKey(elem)) {
      addedItems.remove(elem);
    } else {
      if (!(it instanceof Items.ConstructorItem))
        nDeletions++;
    }

    // change not yet reflected in the trie, don't count it as a deletion

    // we should never be called for local items (they're handled by the scope processor only
    assert !(it instanceof Items.Local);

    if (it instanceof Items.ConstructorItem) {
      // flush constructors array in owning class
      ((Items.CachedConstructorsItem) ((Items.ConstructorItem) it).parent()).invalidateConstructors();
    } else if (it instanceof Items.RefTypeItem) {
      // go through all (local) items and check whether they store a reference to it (rebuild values by item if needed)
      for (Items.Item i : localItems.values()) {
        if (i instanceof Converter.ReferencingItem) {
          byItemNeedsRebuild = byItemNeedsRebuild || ((Converter.ReferencingItem)i).flushReference(it);
        }
      }
    }
  }

  synchronized void changeModifiers(PsiModifierListOwner elem) {
    // find the item
    Items.Item it = lookup(elem, false);
    if (it == null)
      return;

    log("changing the modifiers on " + it + " to " + elem.getModifierList());

    // the psi allows things that are not legal -- we don't
    if (it instanceof Items.SettableFinalItem)
      ((Items.SettableFinalItem)(it)).setFinal(elem.hasModifierProperty(PsiModifier.FINAL));
    else
      log("  can't set final modifier on " + it);

    if (it instanceof Items.SettableStaticItem)
      ((Items.SettableStaticItem)(it)).setStatic(elem.hasModifierProperty(PsiModifier.STATIC));
    else
      log("  can't set static modifier on " + it);
  }

  synchronized void changeTypeParameters(PsiTypeParameterListOwner elem) {
    Items.Item it = lookup(elem, false);
    if (it == null)
      return;

    log("changing the type parameters of " + it + " to " + elem.getTypeParameterList());

    ((Items.CachedTypeParametersItem)it).invalidateTypeParameters();
  }

  synchronized void changeBase(PsiClass elem) {
    Items.Item it = lookup(elem, false);
    if (it == null)
      return;

    log("changing the base class of " + it + " to ");
    assert elem.getExtendsList() != null;
    log(elem.getExtendsList().getReferenceElements());

    ((Items.CachedBaseItem)it).invalidateBase();
    byItemNeedsRebuild = true;
  }

  synchronized void changeImplements(PsiClass elem) {
    Items.Item it = lookup(elem, false);
    if (it == null)
      return;

    log("changing the implements list of " + it + " to " + elem.getImplementsList());

    ((Items.CachedSupersItem)it).invalidateSupers();
    byItemNeedsRebuild = true;
  }

  synchronized void changeParameters(PsiMethod elem) {
    Items.Item it = lookup(elem, false);
    if (it == null)
      return;

    log("changing the implements list of " + it + " to " + elem.getParameterList());

    ((Items.CachedParametersItem)it).invalidateParameters();
  }

  synchronized void changeReturnType(PsiMethod elem) {
    Items.Item it = lookup(elem, false);

    if (it == null)
      return;

    log("changing the return type of " + it + " to " + elem.getReturnType());

    if (it instanceof Items.ConstructorItem) {
      // changing the return type of a constructor always results in it not being a constructor any more
      assert !Converter.isConstructor(elem);
      // add as a method
      deleteItem(elem);
      addItem(elem);
    } else if (Converter.isConstructor(elem)) { // or we just made a constructor by deleting the return type of a method with the same name as its class
      deleteItem(elem);
      addItem(elem);
    } else {
      ((Items.CachedReturnTypeItem)it).invalidateReturnType();
    }
    // TODO: rebuild valuesByItem (once methods are part of it by return type)
  }

  // the name of this item has changed, but references to it remain valid (must be re-inserted into the trie, but no other action necessary)
  synchronized void changeItemName(PsiNamedElement elem) {
    // find the item
    Items.Item it = lookup(elem, false);

    if (it == null)
      return;

    log("changing the name of " + it + " to " + elem.getName());

    if (it instanceof Items.ConstructorItem || elem instanceof PsiMethod && Converter.isConstructor((PsiMethod)elem)) {
      // changing the name of a constructor always results in a method, and we can make constructors by changing the
      // name of a null return type function to the name of its class

      // there may be inner classes of it, but those should only be added in scope scanning

      deleteItem(elem);
      addItem(elem);
    } else {
      // regular name change

      // delete from trie (by overwriting the corresponding stored item with a deleted dummy) and add to addedItems

      // only delete it from the trie if it's already in there
      if (!addedItems.containsKey(elem)) {
        Items.Item dummy = new Items.SimpleTypeVar(it.name());
        dummy.delete();
        dTrie.overwrite(it, dummy);
      }

      ((Items.CachedNameItem)it).refreshName();

      // add it with the new name
      addedItems.put(elem, it);

      // name changes will make references invalid (unresolved, most likely)
      if (it instanceof Items.RefTypeItem) {
        // go through all (local) items and check whether they store a reference to it
        for (Items.Item i : localItems.values())
          if (i instanceof Converter.ReferencingItem)
            byItemNeedsRebuild = byItemNeedsRebuild || ((Converter.ReferencingItem)i).flushReference(it);
      }
    }
  }
}
