package com.eddysystems.eddy;

import ambiguity.JavaUtils;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;
import scala.NotImplementedError;
import scala.collection.JavaConversions;
import tarski.Base;
import tarski.Environment.Env;
import tarski.Environment.PlaceInfo;
import tarski.Items.*;
import tarski.Tarski;
import tarski.Tries;
import tarski.Types.ClassType;
import tarski.Types.Type;

import java.util.*;

import static ambiguity.JavaUtils.popScope;
import static ambiguity.JavaUtils.pushScope;

/**
 * Extracts information about the environment at a given place in the code and makes it available in a format understood by tarski
 */
public class EnvironmentProcessor extends BaseScopeProcessor implements ElementClassHint {

  // a class storing information about the environment.
  static class JavaEnvironment {

    // rebuild the local environment if rebuildRatio of localEnv is deleted, or rebuildRatio of denv is in addedItems
    // (but require at least rebuildMinChanges additions or deletions)
    final static int rebuildMinChanges = 20;
    final static float rebuildRatio = .1f;

    @NotNull final Project project;
    // a converter instance for adding to local*.
    @NotNull final Converter converter;

    JavaEnvironment(@NotNull Project project) {
      this.project = project;
      converter = new Converter(new Place(project, null), this, localItems, localImplicitConstructors);
    }

    // immutable, global (library) environment
    Map<PsiElement, Item> items = new HashMap<PsiElement, Item>();
    Map<PsiClass,ConstructorItem> implicitConstructors = new HashMap<PsiClass, ConstructorItem>();

    // mutable, local (project) environment
    Map<PsiElement, Item> localItems = new HashMap<PsiElement, Item>();
    Map<PsiClass,ConstructorItem> localImplicitConstructors = new HashMap<PsiClass, ConstructorItem>();

    // items added since the last time the local environment was built (to be added to the scope environment)
    Map<PsiElement, Item> addedItems = new HashMap<PsiElement, Item>();
    Map<PsiClass,ConstructorItem> addedImplicitConstructors = new HashMap<PsiClass, ConstructorItem>();
    int nDeletions;

    // pre-computed data structures to enable fast creation of appropriate scala Env instances

    // when a local env is created, it will contain three tries: Globals, Locals, and Volatile, which contains items
    // added to locals (but not in the trie), and the locals found by the EnvironmentProcessor
    Tries.Trie sTrie = null;
    Map<TypeItem,Value[]> sByItem = null;

    Tries.DTrie dTrie = null;
    Map<TypeItem,Value[]> dByItem = null;

    // dynamic by item needs to be rebuilt a lot more than the trie.
    boolean byItemNeedsRebuild = false;

    boolean knows(PsiElement elem) {
      // we need not check for constructors, they're indexed by their class -- we know the constructor iff we know the class.
      return items.containsKey(elem) || localItems.containsKey(elem);
    }

    Item lookup(PsiElement elem) {
      // everything in addedItems is also in localItems.
      Item i = items.get(elem);
      return i == null ? localItems.get(elem) : i;
    }

    ConstructorItem lookupImplicitConstructor(PsiClass elem) {
      // everything in addedImplicitConstructors is also in localImplicitConstructors.
      ConstructorItem i = implicitConstructors.get(elem);
      return i == null ? localImplicitConstructors.get(elem) : i;
    }

    // initialize static environment
    void buildStaticEnv() {
      ArrayList<Item> items = new ArrayList<Item>(this.items.values());
      items.addAll(implicitConstructors.values());
      // add extraEnv items (they're not added in addBase)
      items.addAll(Arrays.asList(Base.extraEnv().allItems()));
      sTrie = Tarski.makeTrie(items);
      sByItem = JavaUtils.valuesByItem(items.toArray(new Item[items.size()]));
    }

    void buildDynamicEnv() {
      ArrayList<Item> items = new ArrayList<Item>(this.localItems.values());
      items.addAll(localImplicitConstructors.values());
      dTrie = Tarski.makeDTrie(items);
      dByItem = JavaUtils.valuesByItem((Item[])dTrie.values());

      // clear volatile stores
      addedImplicitConstructors.clear();
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
    Env getLocalEnvironment(PsiElement place) {
      if (localEnvNeedsRebuild()) {
        buildDynamicEnv();
      } else if (byItemNeedsRebuild) {
        dByItem = JavaUtils.valuesByItem((Item[])dTrie.values());
      }

      return new EnvironmentProcessor(project, place, true).getLocalEnvironment(this);
    }

    // used by the environment processor to make an environment including our precomputed information
    Env makeEnvironment(Collection<Item> scopeItems, Map<Item,Integer> inScope, PlaceInfo pinfo) {
      ArrayList<Item> newitems = new ArrayList<Item>(scopeItems);
      newitems.addAll(addedItems.values());
      newitems.addAll(addedImplicitConstructors.values());
      final Item[] newArray = newitems.toArray(new Item[newitems.size()]);
      /*
      System.out.println("made environment: " + sTrie.values().length + " static, " + dTrie.values().length + " dynamic, " + newArray.length + " volatile.");
      for (Item it : newitems) {
        if (it instanceof Converter.PsiEquivalent)
          System.out.println("  volatile item: " + it + " => " + ((Converter.PsiEquivalent)it).psi() + "@" + ((Converter.PsiEquivalent)it).psi().hashCode() + " original " + ((Converter.PsiEquivalent)it).psi().getOriginalElement().hashCode());
        else
          System.out.println("  volatile item: " + it);
      }
      */
      return Tarski.environment(sTrie, dTrie, Tarski.makeTrie(newArray), sByItem, dByItem, JavaUtils.valuesByItem(newArray), inScope, pinfo);
    }

    // like with the global environment, nothing in the local environment is in scope. It's only about valuesByItem and the trie.

    void addItem(PsiElement elem) {
      // the handler calls this for each interesting element that is added, there is no need for Psi tree
      // traversal in here

      // add to localItems and addedItems
      assert lookup(elem) == null;

      // don't add inaccessible things
      if (converter.place.isInaccessible((PsiModifierListOwner)elem, true))
        return;

      // this adds to localItems and localImplicitConstructors
      Item it = converter.addItem(elem);

      // we may have to remove an implicit constructor if we just added a constructor
      if (it instanceof ConstructorItem)
        deleteImplicitConstructor((PsiClass)(elem.getParent()));

      // save a copy in added*
      addedItems.put(elem,it);
      if (it instanceof ClassItem && localImplicitConstructors.containsKey((PsiClass)elem)) {
        addedImplicitConstructors.put((PsiClass)elem, localImplicitConstructors.get(elem));
      }

      // if we added a class, check if we can fill in some of the undefined references
      if (it instanceof RefTypeItem) {
        // go through all (local) items and check whether we can fill in some undefined references
        // this may add inheritance structure that invalidates valuesByItem
        for (Item i : localItems.values())
          if (i instanceof Converter.ReferencingItem)
            byItemNeedsRebuild = byItemNeedsRebuild || ((Converter.ReferencingItem)i).fillUnresolvedReferences();
      }
    }

    // delete an implicitly defined constructor, if there is any
    void deleteImplicitConstructor(PsiClass cls) {
      if (!localImplicitConstructors.containsKey(cls))
        return;

      ConstructorItem it = localImplicitConstructors.get(cls);
      it.delete();

      localImplicitConstructors.remove(cls);

      // make sure to remove it from added if it hasn't propagated to the Trie yet
      if (addedImplicitConstructors.containsKey(cls))
        addedImplicitConstructors.remove(cls);
    }

    void deleteItem(PsiElement elem) {

      // this is called from beforeDelete, so elem is still valid

      // the handler calls this for each interesting element that will be deleted, there is no need for Psi tree
      // traversal in here

      // any elements in the psi tree below this element have been deleted already, if they needed to be deleted

      Item it = lookup(elem);

      // do we even care?
      if (it == null)
        return;

      System.out.println("deleting " + it);
      it.delete();

      // this must be a local item for this to work
      assert localItems.containsKey(elem);
      localItems.remove(elem);

      // change not yet reflected in the trie, don't count it as a deletion
      if (addedItems.containsKey(elem)) {
        addedItems.remove(elem);
      } else {
        nDeletions++;
      }

      // we should never be called for local items (they're handled by the scope processor only
      assert !(it instanceof LocalVariableItem || it instanceof ParameterItem);

      if (it instanceof EnumConstantItem ||
          it instanceof FieldItem) {
        // nobody should hold references to these types if items, so we should be fine.
      } else if (it instanceof MethodItem) {
        // we may be the parent of local classes, but those classes are only scanned in scope
      } else if (it instanceof ConstructorItem) {
        // we may be the parent of local classes, but those classes are only scanned in scope

        // constructors array in owning class
        ((CachedConstructorsItem) ((ConstructorItem) it).parent()).invalidateConstructors();

        // if this was the last constructor, make an implicit one
        PsiClass cls = (PsiClass)elem.getParent();
        ClassItem cit = (ClassItem)lookup(cls);
        if (cls.getConstructors().length == 1) {
          addedImplicitConstructors.put(cls,converter.addImplicitConstructor(cls,cit));
        }
      } else if (it instanceof PackageItem) {
        // we may be the parent of local classes, but those classes are only scanned in scope
      } else if (it instanceof RefTypeItem) {
        // we may be the parent of local classes (if we're a class), but those classes are only scanned in scope

        // go through all (local) items and check whether they store a reference to it (rebuild values by item if needed)
        for (Item i : localItems.values()) {
          if (i instanceof Converter.ReferencingItem) {
            byItemNeedsRebuild = byItemNeedsRebuild || ((Converter.ReferencingItem)i).flushReference(it);
          }
        }

      } else {
        throw new RuntimeException("don't know what to update for deleted " + elem + ": " + it);
      }
    }

    void changeModifiers(PsiModifierListOwner elem) {
      // find the item
      Item it = lookup(elem);
      if (it == null)
        return;

      System.out.println("changing the modifiers on " + it + " to " + elem.getModifierList());

      // the psi allows things that are not legal -- we don't
      if (it instanceof SettableFinalItem)
        ((SettableFinalItem)(it)).setFinal(elem.hasModifierProperty(PsiModifier.FINAL));
      else
        System.out.println("  can't set final modifier on " + it);

      if (it instanceof SettableStaticItem)
        ((SettableStaticItem)(it)).setStatic(elem.hasModifierProperty(PsiModifier.STATIC));
      else
        System.out.println("  can't set static modifier on " + it);
    }

    void changeTypeParameters(PsiTypeParameterListOwner elem) {
      Item it = lookup(elem);
      if (it == null)
        return;

      System.out.println("changing the type parameters of " + it + " to " + elem.getTypeParameterList());

      ((CachedTypeParametersItem)it).invalidateTypeParameters();
    }

    void changeBase(PsiClass elem) {
      Item it = lookup(elem);
      if (it == null)
        return;

      System.out.print("changing the base class of " + it + " to ");
      for (PsiElement el : elem.getExtendsList().getReferenceElements())
        System.out.print(" " + el);
      System.out.println();

      ((CachedBaseItem)it).invalidateBase();
      byItemNeedsRebuild = true;
    }

    void changeImplements(PsiClass elem) {
      Item it = lookup(elem);
      if (it == null)
        return;

      System.out.println("changing the implements list of " + it + " to " + elem.getImplementsList());

      ((CachedSupersItem)it).invalidateSupers();
      byItemNeedsRebuild = true;
    }

    void changeParameters(PsiMethod elem) {
      Item it = lookup(elem);
      if (it == null)
        return;

      System.out.println("changing the implements list of " + it + " to " + elem.getParameterList());

      ((CachedParametersItem)it).invalidateParameters();
    }

    void changeReturnType(PsiMethod elem) {
      Item it = lookup(elem);
      if (it == null)
        return;

      System.out.println("changing the return type of " + it + " to " + elem.getReturnType());

      if (it instanceof ConstructorItem) {
        // if it is a constructor and we add a return type, we're making it into a method.
        deleteItem(elem);
        addItem(elem);
      } else {
        assert it instanceof MethodItem;
        // if we remove a return type from a method which is called the same as its containing class, we make it into a constructor
        if (elem.getReturnType() == null && elem.getName().equals(((PsiClass)elem.getParent()).getName())) {
          deleteItem(elem);
          addItem(elem);
        } else {
          ((CachedReturnTypeItem)it).invalidateReturnType();
        }
      }

      // TODO: rebuild valuesByItem (once methods are part of it by return type)
    }

    // the name of this item has changed, but references to it remain valid (must be re-inserted into the trie, but no other action necessary)
    void changeItemName(PsiNamedElement elem) {
      // find the item
      Item it = lookup(elem);

      if (it == null)
        return;

      System.out.println("changing the name of " + it + " to " + elem.getName());

      // delete from trie (by overwriting the corresponding stored item with a deleted dummy) and add to addedItems
      Item dummy = new SimpleTypeVar(it.name());
      dummy.delete();
      dTrie.overwrite(it, dummy);

      addedItems.put(elem, it);
      if (localImplicitConstructors.containsKey(elem)) {
        ConstructorItem itc = localImplicitConstructors.get(elem);
        dTrie.overwrite(itc, dummy);
        addedImplicitConstructors.put((PsiClass)elem, itc);
      }

      ((CachedNameItem)it).refreshName();

      // name changes will make references invalid (unresolved, most likely)
      if (it instanceof RefTypeItem) {
        // go through all (local) items and check whether they store a reference to it
        for (Item i : localItems.values()) {
          if (i instanceof Converter.ReferencingItem) {
            byItemNeedsRebuild = byItemNeedsRebuild || ((Converter.ReferencingItem)i).flushReference(it);
          }
        }
      }
    }
  }

  private static final @NotNull Logger logger = Logger.getInstance("EnvironmentProcessor");

  private final @NotNull Place place;
  public class ShadowElement<E> {
    public final E e;
    public final int shadowingPriority;

    public ShadowElement(E e, int p) {
      this.e = e;
      shadowingPriority = p;
    }
  }

  // things that are in scope (not all these are accessible! things may be private, or not static while we are)
  private final List<ShadowElement<PsiPackage>> packages = new SmartList<ShadowElement<PsiPackage>>();
  private final List<ShadowElement<PsiClass>> classes = new SmartList<ShadowElement<PsiClass>>();
  private final List<ShadowElement<PsiVariable>> variables = new SmartList<ShadowElement<PsiVariable>>();
  private final List<ShadowElement<PsiMethod>> methods = new SmartList<ShadowElement<PsiMethod>>();

  // used during walking
  private int currentLevel = 0;
  private boolean inStaticScope = false;
  private PsiElement currentFileContext;
  private boolean honorPrivate;

  public EnvironmentProcessor(@NotNull Project project, PsiElement place, boolean honorPrivate) {
    this.place = new Place(project,place);
    this.honorPrivate = honorPrivate;

    // this is set to null when we go to java.lang
    this.currentFileContext = place;

    // this walks up the PSI tree, but also processes import statements
    PsiScopesUtil.treeWalkUp(this, place, place.getContainingFile());
  }

  static private void addBase(Converter env, GlobalSearchScope scope, boolean noProtected) {
    pushScope("add base");
    try {
      // Extra things don't correspond to PsiElements
      final Set<Item> extra = new HashSet<Item>();
      for (Item i : tarski.Base.extraEnv().allItems())
        extra.add(i);

      // Add classes and packages
      final JavaPsiFacade facade = JavaPsiFacade.getInstance(env.place.project);
      for (Item item : tarski.Base.baseEnv().allItems()) {
        if (extra.contains(item) || item instanceof ConstructorItem)
          continue;
        final String name = item.qualifiedName().get();
        PsiElement psi;
        if (item instanceof PackageItem)
          psi = facade.findPackage(name);
        else if (item instanceof ClassItem)
          psi = facade.findClass(name,scope);
        else
          throw new NotImplementedError("Unknown base type "+item.getClass());
        if (psi == null)
          throw new RuntimeException("Couldn't find "+name);
        //System.out.println("adding base item " + item + " for " + psi + "@" + psi.hashCode() + " original " + psi.getOriginalElement().hashCode());
        env.locals.put(psi,item);
      }

      // Add constructors
      for (Item item : tarski.Base.baseEnv().allItems()) {
        if (!(item instanceof ConstructorItem))
          continue;
        final String clsName = ((ConstructorItem)item).parent().qualifiedName().get();
        final PsiClass cls = facade.findClass(clsName,scope);
        assert cls != null;
        final PsiMethod[] cons = cls.getConstructors();
        if (cons.length != 1)
          logger.warn("found constructors for Object: #" + cons.length);
        env.locals.put(cons[0],item);
      }

      // Add class members
      for (Item item : tarski.Base.baseEnv().allItems()) {
        if (extra.contains(item) || !(item instanceof ClassItem))
          continue;
        final String name = item.qualifiedName().get();
        env.addClassMembers(facade.findClass(name,scope),(ClassItem)item,noProtected);
      }
    } finally { popScope(); }
  }

  protected static void storeClassInfo(Place place, GlobalSearchScope scope, JavaEnvironment lookup, Map<PsiElement,Item> items, Map<PsiClass,ConstructorItem> implicitConstructors, boolean doAddBase) {
    final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(place.project);
    final Converter env = new Converter(place,lookup,items,implicitConstructors);

    if (doAddBase)
      addBase(env,scope,true);

    for (String name : cache.getAllClassNames()) {
      // keep IDE responsive
      Utility.processEvents();

      for (PsiClass cls : cache.getClassesByName(name, scope)) {
        if (!doAddBase)
          System.out.println("processing class (" + (doAddBase ? "global" : "local") + "): " + cls.getQualifiedName() + ", accessible: " + !place.isInaccessible(cls, true));
        if (!place.isInaccessible(cls, true))
          env.addClass(cls, true, true);
      }
    }
  }

  protected static JavaEnvironment getEnvironment(Project project) {
    final JavaEnvironment jenv = new JavaEnvironment(project);

    pushScope("make base environment");
    try {
      final Place place = new Place(project, null);

      final GlobalSearchScope librariesScope = ProjectScope.getLibrariesScope(place.project);
      storeClassInfo(place, librariesScope, jenv, jenv.items, jenv.implicitConstructors, true);
      logger.info("making static environment with " + jenv.items.size() + " items.");
      jenv.buildStaticEnv();

      final GlobalSearchScope projectScope = ProjectScope.getProjectScope(place.project);
      storeClassInfo(place, projectScope, jenv, jenv.localItems, jenv.localImplicitConstructors, false);
      logger.info("making dynamic environment with " + jenv.localItems.size() + " items.");
      jenv.buildDynamicEnv();

    } finally { popScope(); }

    return jenv;
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarski engine to look up possible names, using jenv as a base
   */
  private Env getLocalEnvironment(JavaEnvironment jenv) {
    // local variables, parameters, type parameters, as well as protected/private things in scope
    final Map<PsiElement,Item> locals = new HashMap<PsiElement, Item>();
    final Map<PsiClass,ConstructorItem> localImplicitConstructors = new HashMap<PsiClass, ConstructorItem>();
    final Map<Item,Integer> scopeItems = new HashMap<Item, Integer>();
    final Converter env = new Converter(place,jenv,locals,localImplicitConstructors);

    logger.info("getting local items...");

    // register locally visible items
    for (ShadowElement<PsiPackage> spkg : packages) {
      final PsiPackage pkg = spkg.e;
      final Item ipkg = env.addContainer(pkg);
      scopeItems.put(ipkg,spkg.shadowingPriority);
    }

    // then, register classes (we need those as containing elements in the other things)
    // classes may be contained in classes, so partial-order the list first
    for (ShadowElement<PsiClass> scls : classes) {
      final PsiClass cls = scls.e;
      final Item icls = env.addClass(cls, true, false);
      scopeItems.put(icls,scls.shadowingPriority);
    }

    // register methods (also register types used in this method)
    for (ShadowElement<PsiMethod> smethod : methods) {
      final PsiMethod method = smethod.e;
      final Item imethod = env.addMethod(method);
      scopeItems.put(imethod,smethod.shadowingPriority);
    }

    // then, register objects which have types (enum constants, variables, parameters, fields), and their types
    for (ShadowElement<PsiVariable> svar : variables) {
      final PsiVariable var = svar.e;
      if (var instanceof PsiField) {
        final Item ivar = env.addField((PsiField) var);
        scopeItems.put(ivar,svar.shadowingPriority);
      } else {
        assert !jenv.knows(var);
        assert !locals.containsKey(var);
        final Type t = env.convertType(var.getType());
        final boolean isFinal = var.hasModifierProperty(PsiModifier.FINAL);
        final Item i = var instanceof PsiParameter     ? new ParameterItem(var.getName(),t,isFinal)
                     : var instanceof PsiLocalVariable ? new LocalVariableItem(var.getName(),t,isFinal)
                     : null;
        if (i == null)
          throw new scala.NotImplementedError("Unknown variable: " + var);

        // Actually add to locals
        locals.put(var, i);
        scopeItems.put(i,svar.shadowingPriority);
      }
    }

    logger.info("added " + locals.size() + " locals");

    final List<Item> local_items = new ArrayList<Item>();
    local_items.addAll(locals.values());
    local_items.addAll(localImplicitConstructors.values());

    // find out which element we are inside (method, class or interface, or package)
    ParentItem placeItem = null;
    boolean inside_continuable = false;
    boolean inside_breakable = false;
    final List<String> labels = new SmartList<String>();
    // walk straight up until we see a method, class, or package
    PsiElement place = this.place.place;
    while (place != null) {
      // scan the current method for labels, loops, and switch statements
      if (placeItem != null) {
        if (place instanceof PsiLabeledStatement) {
          // found a label
          logger.info("found a labeled statement: " + place + ", label: " + ((PsiLabeledStatement) place).getLabelIdentifier());
          labels.add(((PsiLabeledStatement) place).getLabelIdentifier().getText());
        }

        if (place instanceof PsiSwitchStatement) {
          logger.info("inside switch statement: " + place);
          inside_breakable = true;
        }
        if (place instanceof PsiLoopStatement) {
          logger.info("inside loop statement: " + place);
          inside_breakable = true;
          inside_continuable = true;
        }
      }

      // add special "this" and "super" items this for each class we're inside of, with same shadowing priority as the class itself
      if (place instanceof PsiClass && !((PsiClass) place).isInterface()) { // don't make this for interfaces
        assert locals.containsKey(place) || jenv.knows(place);
        final ClassItem c = (ClassItem)env.addClass((PsiClass)place, false, false);
        assert scopeItems.containsKey(c);
        final int p = scopeItems.get(c);
        final ThisItem ti = new ThisItem(c);
        local_items.add(ti);
        scopeItems.put(ti,p);

        final ClassType s = c.base();
        assert s.item().isClass();
        final SuperItem si = new SuperItem(s);
        local_items.add(si);
        scopeItems.put(si,p);
      }

      if (place instanceof PsiMethod || place instanceof PsiClass || place instanceof PsiPackage) {
        if (placeItem == null) {
          if (jenv.knows(place))
            placeItem = (ParentItem)jenv.items.get(place);
          else if (locals.containsKey(place))
            placeItem = (ParentItem)locals.get(place);
          else
            assert false : "cannot find placeItem";
        }
      } else if (place instanceof PsiJavaFile) {
        final PsiPackage pkg = this.place.getPackage((PsiJavaFile) place);
        if (pkg == null) {
          // probably we're top-level in a file without package statement, use LocalPackageItem
          if (placeItem == null)
            placeItem = Tarski.localPkg();
        } else {
          if (placeItem == null) {
            assert locals.containsKey(pkg) || jenv.knows(pkg);
            placeItem = (ParentItem)env.addContainer(pkg);
          }
        }
        break;
      }
      place = place.getParent();
    }
    assert placeItem != null;

    logger.info("environment (" + local_items.size() + " local items) taken inside " + placeItem + ", making env");

    return jenv.makeEnvironment(local_items, scopeItems, new PlaceInfo(placeItem, inside_breakable, inside_continuable, JavaConversions.asScalaBuffer(labels).toList()));
  }

  @Override
  public boolean shouldProcess(DeclarationKind kind) {
    return
      kind == DeclarationKind.CLASS ||
      kind == DeclarationKind.FIELD ||
      kind == DeclarationKind.METHOD ||
      kind == DeclarationKind.VARIABLE ||
      kind == DeclarationKind.PACKAGE ||
      kind == DeclarationKind.ENUM_CONST;
  }

  @Override
  public boolean execute(@NotNull PsiElement element, ResolveState state) {

    // if we are in static scope, a class member has to be declared static for us to see it
    // TODO: we should add these either way, and let the semantics logic take care of static scoping
    if (element instanceof PsiField || element instanceof PsiMethod) {
      if (inStaticScope && !((PsiMember)element).hasModifierProperty(PsiModifier.STATIC))
        return true;
    }

    if (honorPrivate && place.isInaccessible((PsiModifierListOwner)element, false)) {
      logger.debug("rejecting " + element + " because it is inaccessible");
      return true;
    }

    logger.debug("found element " + element + " at level " + currentLevel);

    if (element instanceof PsiClass) {
      classes.add(new ShadowElement<PsiClass>((PsiClass)element, currentLevel));
    } else if (element instanceof PsiVariable) {
      variables.add(new ShadowElement<PsiVariable>((PsiVariable)element, currentLevel));
    } else if (element instanceof PsiMethod) {
      methods.add(new ShadowElement<PsiMethod>((PsiMethod)element, currentLevel));
    } else if (element instanceof PsiPackage) {
      packages.add(new ShadowElement<PsiPackage>((PsiPackage)element, currentLevel));
    }
    return true;
  }

  @Override
  public final void handleEvent(@NotNull Event event, Object associated){
    if (event == JavaScopeProcessorEvent.START_STATIC) {
      logger.debug("starting static scope");
      inStaticScope = true;
    } else if (event == JavaScopeProcessorEvent.SET_CURRENT_FILE_CONTEXT) {
      currentFileContext = (PsiElement)associated;
      logger.debug("switching file context: " + currentFileContext);
    } else if (event == JavaScopeProcessorEvent.CHANGE_LEVEL) {
      currentLevel++;
      logger.debug("change level to " + currentLevel);
    }
  }
}
