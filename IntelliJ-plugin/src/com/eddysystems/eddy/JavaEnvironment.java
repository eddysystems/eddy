package com.eddysystems.eddy;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import tarski.*;

import java.util.*;

import static ambiguity.JavaUtils.popScope;
import static ambiguity.JavaUtils.pushScope;
import static com.eddysystems.eddy.Utility.log;

// a class storing information about the environment.
public class JavaEnvironment {

  static class NoJDKError extends RuntimeException {
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
    final IdFilter filter = new IdFilter() {
      @Override
      public boolean containsFileId(int id) {
        return true;
      }
    };
    final Place place;
    final Converter converter;
    final List<Items.Item> results = new ArrayList<Items.Item>();

    final Processor<PsiClass> classProc = new Processor<PsiClass>() {
      @Override
      public boolean process(PsiClass cls) {
        if (!place.isInaccessible(cls, true))
          results.add(converter.addClass(cls, false, true));
        return true;
      }
    };

    final Processor<PsiMethod> methodProc = new Processor<PsiMethod>() {
      @Override
      public boolean process(PsiMethod method) {
        if (!place.isInaccessible(method, true) && !Converter.isConstructor(method))
          results.add(converter.addMethod(method));
        return true;
      }
    };

    final Processor<PsiField> fieldProc = new Processor<PsiField>() {
      @Override
      public boolean process(PsiField fld) {
        if (!place.isInaccessible(fld, true))
          results.add(converter.addField(fld));
        return true;
      }
    };

    PsiGenerator(Project project, GlobalSearchScope scope, Converter conv) {
      this.project = project;
      this.scope = scope;
      psicache = PsiShortNamesCache.getInstance(project);
      place = new Place(project, null);
      converter = conv;
    }

    private Items.Item[] generate(String s) {
      results.clear();
      psicache.processClassesWithName(s, classProc, scope, filter);
      psicache.processMethodsWithName(s, methodProc, scope, filter);
      psicache.processFieldsWithName(s, fieldProc, scope, filter);
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
  @NotNull final Tries.LazyTrie<Items.Item> sTrie;

  // global (library) environment. Only added to, never deleted from or changed. All items not in project files go in here
  Map<PsiElement, Items.Item> items = new HashMap<PsiElement, Items.Item>();

  // mutable, local (project) environment, can be added to and changed by edits. All items in project files go in here
  Map<PsiElement, Items.Item> localItems = new HashMap<PsiElement, Items.Item>();

  // items added since the last time the local environment was built (to be added to the scope environment)
  Map<PsiElement, Items.Item> addedItems = new HashMap<PsiElement, Items.Item>();
  int nDeletions;

  // items found in scope by the EnvironmentProcessor
  Map<PsiElement, Items.Item> scopeItems = new HashMap<PsiElement, Items.Item>();

  // pre-computed data structures to enable fast creation of appropriate scala Env instances

  // when a local env is created, it contains: A global lookup object (created from the global list of all names and a generator object
  // able to translate a name into a list of items), a trie storing Local items, a byItem map for local items, and a trie and byItem map
  // containing items added to locals (but not in the trie yet), and the locals found by the EnvironmentProcessor.
  Tries.DTrie<Items.Item> dTrie = null;
  Map<Items.TypeItem, Items.Value[]> dByItem = null;

  // dynamic by item needs to be rebuilt a lot more than the trie.
  boolean byItemNeedsRebuild = false;

  JavaEnvironment(@NotNull Project project) {
    this.project = project;
    converter = new Converter(new Place(project, null), this, items, localItems, addedItems);

    pushScope("make base environment");
    try {
      // add base items
      addBase();

      // store all project items and their dependencies
      storeProjectClassInfo();
      buildDynamicEnv();

      // make global item generator
      sTrie = prepareLazyTrie();
    } finally { popScope(); }

  }

  boolean knows(PsiElement elem) {
    return items.containsKey(elem) || localItems.containsKey(elem) || scopeItems.containsKey(elem);
  }

  @Nullable
  Items.Item lookup(PsiElement elem) {
    // everything in addedItems is also in localItems.
    Items.Item i = items.get(elem);
    if (i == null)
      i = localItems.get(elem);
    if (i == null)
      i = scopeItems.get(elem);
    return i;
  }

  private void addBase() {
    final GlobalSearchScope scope = ProjectScope.getAllScope(project);
    final boolean noProtected = true;

    pushScope("add base");
    try {
      // Extra things don't correspond to PsiElements
      final Set<Items.Item> extra =  new HashSet<Items.Item>();
      Collections.addAll(extra, Base.extraEnv().allItems());

      // Add classes and packages
      final JavaPsiFacade facade = JavaPsiFacade.getInstance(project);
      for (Items.Item item : tarski.Base.baseEnv().allItems()) {
        if (extra.contains(item) || item instanceof Items.ConstructorItem)
          continue;
        final String name = item.qualifiedName().get();
        PsiElement psi;
        if (item instanceof Items.PackageItem)
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
        final String clsName = ((Items.ConstructorItem)item).parent().qualifiedName().get();
        final PsiClass cls = facade.findClass(clsName,scope);
        assert cls != null;
        final PsiMethod[] cons = cls.getConstructors();
        if (cons.length != 1)
          log("found " + cons.length + " constructors for Object " + cls);
        converter.put(cons[0],item);
      }

      // Add class members
      for (Items.Item item : tarski.Base.baseEnv().allItems()) {
        if (extra.contains(item) || !(item instanceof Items.ClassItem))
          continue;
        final String name = item.qualifiedName().get();
        converter.addClassMembers(facade.findClass(name, scope), (Items.ClassItem) item, noProtected);
      }
    } finally { popScope(); }
  }

  private Tries.LazyTrie<Items.Item> prepareLazyTrie() {
    String[] classNames = PsiShortNamesCache.getInstance(project).getAllClassNames();
    String[] fieldNames = PsiShortNamesCache.getInstance(project).getAllFieldNames();
    String[] methodNames = PsiShortNamesCache.getInstance(project).getAllMethodNames();
    String[] allNames = new String[classNames.length + fieldNames.length + methodNames.length];

    System.arraycopy(classNames, 0, allNames, 0, classNames.length);
    System.arraycopy(fieldNames, 0, allNames, classNames.length, fieldNames.length);
    System.arraycopy(methodNames, 0, allNames, classNames.length+fieldNames.length, methodNames.length);

    // there may be duplicates, but we don't care
    Arrays.sort(allNames);

    return new Tries.LazyTrie<Items.Item>(JavaTrie.makeTrieStructure(allNames), new PsiGenerator(project, ProjectScope.getLibrariesScope(project), converter));
  }

  // store all classes and their member in the given scope
  private void storeProjectClassInfo() {
    final GlobalSearchScope scope = ProjectScope.getContentScope(project);
    final Place place = new Place(project, null);
    final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
    final IdFilter filter = IdFilter.getProjectIdFilter(project, false);
    final Processor<PsiClass> proc = new Processor<PsiClass>() {
      @Override
      public boolean process(PsiClass cls) {
        if (!place.isInaccessible(cls, true))
          converter.addClass(cls, true, true);
        // keep IDE alive
        Utility.processEvents();
        return true;
      }
    };

    cache.processAllClassNames(new Processor<String>() {
      @Override
      public boolean process(String name) {
        cache.processClassesWithName(name, proc, scope, filter);
        return true;
      }
    }, scope, filter);
  }

  List<Items.Item> removeConstructors(Collection<Items.Item> items) {
    List<Items.Item> newitems = new ArrayList<Items.Item>(items.size());
    for (Items.Item i : items)
      if (!(i instanceof Items.ConstructorItem))
        newitems.add(i);
    return newitems;
  }

  void buildDynamicEnv() {
    dTrie = Tarski.makeDTrie(removeConstructors(localItems.values()));
    dByItem = JavaItems.valuesByItem(dTrie.values());

    // clear volatile stores
    addedItems.clear();
    nDeletions = 0;
  }

  boolean localEnvNeedsRebuild() {
    int changes = Math.max(nDeletions, addedItems.size());
    if (changes < rebuildMinChanges)
      return false;
    else
      return (float)changes / dTrie.values().length > rebuildRatio;
  }

  // get a combined environment at the given place
  Environment.Env getLocalEnvironment(PsiElement place) {
    if (localEnvNeedsRebuild()) {
      buildDynamicEnv();
    } else if (byItemNeedsRebuild) {
      dByItem = JavaItems.valuesByItem(dTrie.values());
    }

    // ep will fill scopeItems (and it has its own store for special non-psi items and constructors)
    EnvironmentProcessor ep = new EnvironmentProcessor(project, this, scopeItems, place, true);

    List<Items.Item> newitems = removeConstructors(ep.localItems);
    newitems.addAll(addedItems.values()); // addedItems never contains constructors (see Converter.put() and changeItemName())

    final Items.Item[] newArray = newitems.toArray(new Items.Item[newitems.size()]);
    return Tarski.environment(sTrie, dTrie, Tarski.makeTrie(newArray), dByItem, JavaItems.valuesByItem(newArray), ep.scopeItems, ep.placeInfo);
  }


  void addLocalItem(PsiElement elem) {
    // the handler calls this for each interesting element that is added, there is no need for Psi tree
    // traversal in here

    // add to localItems and addedItems
    assert lookup(elem) == null;

    // don't add inaccessible things
    if (converter.place.isInaccessible((PsiModifierListOwner)elem, true))
      return;

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

  void deleteItem(PsiElement elem) {

    // this is called from beforeDelete, so elem is still valid

    // the handler calls this for each interesting element that will be deleted, there is no need for Psi tree
    // traversal in here

    // any elements in the psi tree below this element have been deleted already, if they needed to be deleted

    Items.Item it = lookup(elem);

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

  void changeModifiers(PsiModifierListOwner elem) {
    // find the item
    Items.Item it = lookup(elem);
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

  void changeTypeParameters(PsiTypeParameterListOwner elem) {
    Items.Item it = lookup(elem);
    if (it == null)
      return;

    log("changing the type parameters of " + it + " to " + elem.getTypeParameterList());

    ((Items.CachedTypeParametersItem)it).invalidateTypeParameters();
  }

  void changeBase(PsiClass elem) {
    Items.Item it = lookup(elem);
    if (it == null)
      return;

    log("changing the base class of " + it + " to ");
    assert elem.getExtendsList() != null;
    log(elem.getExtendsList().getReferenceElements());

    ((Items.CachedBaseItem)it).invalidateBase();
    byItemNeedsRebuild = true;
  }

  void changeImplements(PsiClass elem) {
    Items.Item it = lookup(elem);
    if (it == null)
      return;

    log("changing the implements list of " + it + " to " + elem.getImplementsList());

    ((Items.CachedSupersItem)it).invalidateSupers();
    byItemNeedsRebuild = true;
  }

  void changeParameters(PsiMethod elem) {
    Items.Item it = lookup(elem);
    if (it == null)
      return;

    log("changing the implements list of " + it + " to " + elem.getParameterList());

    ((Items.CachedParametersItem)it).invalidateParameters();
  }

  void changeReturnType(PsiMethod elem) {
    Items.Item it = lookup(elem);

    if (it == null)
      return;

    log("changing the return type of " + it + " to " + elem.getReturnType());

    if (it instanceof Items.ConstructorItem) {
      // changing the return type of a constructor always results in it not being a constructor any more
      assert !Converter.isConstructor(elem);
      // add as a method
      deleteItem(elem);
      addLocalItem(elem);
    } else if (Converter.isConstructor(elem)) { // or we just made a constructor by deleting the return type of a method with the same name as its class
      deleteItem(elem);
      addLocalItem(elem);
    } else {
      ((Items.CachedReturnTypeItem)it).invalidateReturnType();
    }
    // TODO: rebuild valuesByItem (once methods are part of it by return type)
  }

  // the name of this item has changed, but references to it remain valid (must be re-inserted into the trie, but no other action necessary)
  void changeItemName(PsiNamedElement elem) {
    // find the item
    Items.Item it = lookup(elem);

    if (it == null)
      return;

    log("changing the name of " + it + " to " + elem.getName());

    if (it instanceof Items.ConstructorItem || elem instanceof PsiMethod && Converter.isConstructor((PsiMethod)elem)) {
      // changing the name of a constructor always results in a method, and we can make constructors by changing the
      // name of a null return type function to the name of its class

      // there may be inner classes of it, but those should only be added in scope scanning

      deleteItem(elem);
      addLocalItem(elem);
    } else {
      // regular name change

      // delete from trie (by overwriting the corresponding stored item with a deleted dummy) and add to addedItems
      Items.Item dummy = new Items.SimpleTypeVar(it.name());
      dummy.delete();
      dTrie.overwrite(it, dummy);

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
