package com.eddysystems.eddy;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.util.Processor;
import com.intellij.util.SmartList;
import com.intellij.util.indexing.IdFilter;
import org.jetbrains.annotations.NotNull;
import scala.NotImplementedError;
import scala.collection.JavaConversions;
import tarski.*;
import tarski.Environment.Env;
import tarski.Environment.PlaceInfo;
import tarski.Items.*;
import tarski.Types.ClassType;
import tarski.Types.Type;

import java.util.*;

import static ambiguity.JavaUtils.popScope;
import static ambiguity.JavaUtils.pushScope;
import static com.eddysystems.eddy.Utility.log;

/**
 * Extracts information about the environment at a given place in the code and makes it available in a format understood by tarski
 */
public class EnvironmentProcessor extends BaseScopeProcessor implements ElementClassHint {

  static class NoJDKError extends RuntimeException {
    NoJDKError(String s) {
      super("No JDK found: " + s);
    }
  }

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
      final Set<Item> extra =  new HashSet<Item>();
      Collections.addAll(extra, Base.extraEnv().allItems());

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
          throw new NoJDKError("Couldn't find " + name);
        //log("adding base item " + item + " for " + psi + "@" + psi.hashCode() + " original " + psi.getOriginalElement().hashCode());
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
          log("found " + cons.length + " constructors for Object " + cls);
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

  static int counter;
  protected static void storeClassInfo(final Place place, final GlobalSearchScope scope, JavaEnvironment lookup, Map<PsiElement,Item> items, Map<PsiMethod,ConstructorItem> cons, boolean doAddBase) {
    counter = 0;
    final PsiShortNamesCache cache = PsiShortNamesCache.getInstance(place.project);
    final Converter env = new Converter(place,lookup,items,cons);

    if (doAddBase)
      addBase(env,scope,true);

    final IdFilter filter = IdFilter.getProjectIdFilter(place.project, true);
    final Processor<PsiClass> proc = new Processor<PsiClass>() {
      @Override
      public boolean process(PsiClass cls) {
        if (!place.isInaccessible(cls, true))
          env.addClass(cls, true, true);
        // keep IDE alive
        Utility.processEvents();
        return true;
      }
    };

    cache.processAllClassNames(new Processor<String>() {
      @Override
      public boolean process(String name) {
        if (counter++ % 100 == 0) {
          log("processing classname: " + name + ", free memory: " + Runtime.getRuntime().freeMemory());
        }

        // if we're low on memory, squeeze the most out of it
        long mem = Runtime.getRuntime().freeMemory();
        if (mem < 50000) {
          PsiManager.getInstance(place.project).dropResolveCaches();
          log("low memory: " + mem + ", dropping resolve caches.");
        }

        cache.processClassesWithName(name, proc, scope, filter);
        return true;
      }
    }, scope, filter);
  }

  protected static JavaEnvironment getEnvironment(Project project) {
    final JavaEnvironment jenv = new JavaEnvironment(project);

    pushScope("make base environment");
    try {
      final Place place = new Place(project, null);

      final GlobalSearchScope librariesScope = ProjectScope.getLibrariesScope(place.project);
      storeClassInfo(place, librariesScope, jenv, jenv.items, jenv.cons, true);
      log("making static environment with " + jenv.items.size() + " items.");
      jenv.buildStaticEnv();

      final GlobalSearchScope projectScope = ProjectScope.getProjectScope(place.project);
      storeClassInfo(place, projectScope, jenv, jenv.localItems, jenv.localCons, false);
      log("making dynamic environment with " + jenv.localItems.size() + " items.");
      jenv.buildDynamicEnv();

    } finally { popScope(); }

    return jenv;
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarski engine to look up possible names, using jenv as a base
   */
  protected Env getLocalEnvironment(JavaEnvironment jenv) {
    // local variables, parameters, type parameters, as well as protected/private things in scope
    final Map<PsiElement,Item> locals = new HashMap<PsiElement, Item>();
    final Map<PsiMethod,ConstructorItem> localCons = new HashMap<PsiMethod, ConstructorItem>();
    final Map<Item,Integer> scopeItems = new HashMap<Item, Integer>();
    final Converter env = new Converter(place,jenv,locals,localCons);

    log("getting local items...");

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
        final Item i = new Local(var.getName(),t,isFinal);

        // Actually add to locals
        locals.put(var, i);
        scopeItems.put(i,svar.shadowingPriority);
      }
    }

    log("added " + locals.size() + " locals");

    final List<Item> local_items = new ArrayList<Item>();
    local_items.addAll(locals.values());

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
          log("found a labeled statement: " + place + ", label: " + ((PsiLabeledStatement) place).getLabelIdentifier());
          labels.add(((PsiLabeledStatement) place).getLabelIdentifier().getText());
        }

        if (place instanceof PsiSwitchStatement) {
          log("inside switch statement: " + place);
          inside_breakable = true;
        }
        if (place instanceof PsiLoopStatement) {
          log("inside loop statement: " + place);
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
            placeItem = (ParentItem)jenv.lookup(place);
          else if (locals.containsKey(place))
            placeItem = (ParentItem)locals.get(place);
          else if (place instanceof PsiMethod && localCons.containsKey(place))
            placeItem = localCons.get(place);
          else
            assert false : "cannot find placeItem " + place;
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
            placeItem = env.addContainer(pkg);
          }
        }
        break;
      }
      place = place.getParent();
    }
    assert placeItem != null;

    log("environment (" + local_items.size() + " local items) taken inside " + placeItem + ", making env");

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
      //log("rejecting " + element + " because it is inaccessible");
      return true;
    }

    //log("found element " + element + " at level " + currentLevel);

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
      log("starting static scope");
      inStaticScope = true;
    } else if (event == JavaScopeProcessorEvent.SET_CURRENT_FILE_CONTEXT) {
      currentFileContext = (PsiElement)associated;
      log("switching file context: " + currentFileContext);
    } else if (event == JavaScopeProcessorEvent.CHANGE_LEVEL) {
      currentLevel++;
      log("change level to " + currentLevel);
    }
  }
}
