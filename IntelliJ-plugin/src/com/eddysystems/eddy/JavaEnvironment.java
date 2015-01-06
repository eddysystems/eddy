package com.eddysystems.eddy;

import ambiguity.JavaUtils;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import org.jetbrains.annotations.NotNull;
import tarski.*;

import java.util.*;

import static com.eddysystems.eddy.Utility.log;

/**
* Created by martin on 06.01.15.
*/ // a class storing information about the environment.
class JavaEnvironment {

  // rebuild the local environment if rebuildRatio of localEnv is deleted, or rebuildRatio of denv is in addedItems
  // (but require at least rebuildMinChanges additions or deletions)
  final static int rebuildMinChanges = 20;
  final static float rebuildRatio = .1f;

  @NotNull
  final Project project;
  @NotNull final Converter converter;

  JavaEnvironment(@NotNull Project project) {
    this.project = project;
    converter = new Converter(new Place(project, null), this, items, cons, localItems, localCons);
  }

  // global (library) environment. Only added to, never deleted from or changed. All items not in project files go in here
  Map<PsiElement, Items.Item> items = new HashMap<PsiElement, Items.Item>();
  Map<PsiMethod, Items.ConstructorItem> cons = new HashMap<PsiMethod, Items.ConstructorItem>();

  // mutable, local (project) environment, can be added to and changed by edits. All items in project files go in here
  Map<PsiElement, Items.Item> localItems = new HashMap<PsiElement, Items.Item>();
  Map<PsiMethod, Items.ConstructorItem> localCons = new HashMap<PsiMethod, Items.ConstructorItem>();

  // items added since the last time the local environment was built (to be added to the scope environment)
  Map<PsiElement, Items.Item> addedItems = new HashMap<PsiElement, Items.Item>();
  int nDeletions;

  // pre-computed data structures to enable fast creation of appropriate scala Env instances

  // when a local env is created, it will contain three tries: Globals, Locals, and Volatile, which contains items
  // added to locals (but not in the trie), and the locals found by the EnvironmentProcessor
  Tries.Trie<Items.Item> sTrie = null;
  Map<Items.TypeItem, Items.Value[]> sByItem = null;

  Tries.DTrie<Items.Item> dTrie = null;
  Map<Items.TypeItem, Items.Value[]> dByItem = null;

  // dynamic by item needs to be rebuilt a lot more than the trie.
  boolean byItemNeedsRebuild = false;

  boolean knows(PsiElement elem) {
    // we need not check for constructors, they're indexed by their class -- we know the constructor iff we know the class.
    return items.containsKey(elem) || localItems.containsKey(elem);
  }

  Items.Item lookup(PsiElement elem) {
    // everything in addedItems is also in localItems.
    Items.Item i = items.get(elem);
    if (i == null)
      i = localItems.get(elem);
    if (i == null && elem instanceof PsiMethod) {
      // for methods, also try to look them up in the constructors
      return lookupConstructor((PsiMethod)elem);
    }
    return i;
  }

  Items.ConstructorItem lookupConstructor(PsiMethod elem) {
    Items.ConstructorItem i = cons.get(elem);
    if (i == null)
      i = localCons.get(elem);
    return i;
  }

  // initialize static environment
  void buildStaticEnv() {
    ArrayList<Items.Item> items = new ArrayList<Items.Item>(this.items.values());
    // add extraEnv items (they're not added in addBase)
    items.addAll(Arrays.asList(Base.extraEnv().allItems()));
    sTrie = Tarski.makeTrie(items);
    sByItem = JavaUtils.valuesByItem(items.toArray(new Items.Item[items.size()]));
  }

  void buildDynamicEnv() {
    ArrayList<Items.Item> items = new ArrayList<Items.Item>(this.localItems.values());
    dTrie = Tarski.makeDTrie(items);
    dByItem = JavaUtils.valuesByItem(dTrie.values());

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
      dByItem = JavaUtils.valuesByItem(dTrie.values());
    }

    return new EnvironmentProcessor(project, place, true).getLocalEnvironment(this);
  }

  // used by the environment processor to make an environment including our precomputed information
  Environment.Env makeEnvironment(Collection<Items.Item> scopeItems, Map<Items.Item,Integer> inScope, Environment.PlaceInfo pinfo) {
    ArrayList<Items.Item> newitems = new ArrayList<Items.Item>(scopeItems);
    newitems.addAll(addedItems.values());
    final Items.Item[] newArray = newitems.toArray(new Items.Item[newitems.size()]);
    return Tarski.environment(sTrie, dTrie, Tarski.makeTrie(newArray), sByItem, dByItem, JavaUtils.valuesByItem(newArray), inScope, pinfo);
  }

  // like with the global environment, nothing in the local environment is in scope. It's only about valuesByItem and the trie.

  void addLocalItem(PsiElement elem) {
    // the handler calls this for each interesting element that is added, there is no need for Psi tree
    // traversal in here

    // add to localItems and addedItems
    assert lookup(elem) == null;

    // don't add inaccessible things
    if (converter.place.isInaccessible((PsiModifierListOwner)elem, true))
      return;

    // this adds to localItems
    Items.Item it = converter.addItem(elem);

    // save a copy in addedItems (constructors don't go there)
    if (it instanceof Items.ConstructorItem) {
     ((Items.CachedConstructorsItem)((Items.ConstructorItem) it).parent()).invalidateConstructors();
    } else {
      addedItems.put(elem,it);
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

    if (it instanceof Items.ConstructorItem) {
      assert elem instanceof PsiMethod;
      localCons.remove(elem);
    } else {
      localItems.remove(elem);
      if (addedItems.containsKey(elem)) {
        addedItems.remove(elem);
      } else {
        nDeletions++;
      }
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
