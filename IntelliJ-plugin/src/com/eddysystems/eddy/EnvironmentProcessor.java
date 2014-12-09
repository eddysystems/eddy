package com.eddysystems.eddy;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectAndLibrariesScope;
import com.intellij.psi.search.PsiShortNamesCache;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import scala.Option;
import scala.Some;
import scala.collection.JavaConversions;
import scala.collection.immutable.Map$;
import tarski.Environment.Env;
import tarski.Items.*;
import tarski.Tarski;
import tarski.Types.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Extracts information about the environment at a given place in the code and makes it available in a format understood by tarski
 */
public class EnvironmentProcessor extends BaseScopeProcessor implements ElementClassHint {

  private final @NotNull Place place;

  private final @NotNull Logger logger = Logger.getInstance(getClass());

  public class ShadowElement<E> {
    public final E e;
    public final int shadowingPriority;

    public ShadowElement(E e, int p) {
      this.e = e;
      shadowingPriority = p;
    }
  }

  // a cache containing all the items in the global environment (everything outside this file)
  // if the PSI referenced here changes, this map becomes useless (we can check with PsiElement.isValid())
  static final Object globals_lock = new Object();
  static boolean globals_ready = false;
  static Map<PsiElement, Item> globals = null;
  static Env global_env = null;

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

  // setup only for statics initialization -- the result of this cannot be used
  private EnvironmentProcessor(@NotNull Project project) {
    this.place = new Place(project,null);
    getGlobals();
  }

  static public void initGlobalEnvironment(@NotNull Project project) {
    new EnvironmentProcessor(project);
  }

  static public void clearGlobalEnvironment() {
    synchronized (globals_lock) {
      globals_ready = false;
      global_env = null;
      globals = null;
    }
  }

  public EnvironmentProcessor(@NotNull Project project, PsiElement place, boolean honorPrivate) {
    this.place = new Place(project,place);
    this.honorPrivate = honorPrivate;

    // this is set to null when we go to java.lang
    this.currentFileContext = place;

    logger.setLevel(Level.DEBUG);

    // this walks up the PSI tree, but also processes import statements
    PsiScopesUtil.treeWalkUp(this, place, place.getContainingFile());
  }

  private Map<PsiElement,Item> getGlobals() {
    if (globals_ready) {
      return globals;
    } else {

      synchronized (globals_lock) {

        if (globals == null) {
          // get all classes from IntelliJ
          globals = new HashMap<PsiElement, Item>();
        }

        logger.info("making globals (" + globals.size() + " items already there)");

        PsiShortNamesCache cache = PsiShortNamesCache.getInstance(place.project);
        String[] classnames = cache.getAllClassNames();
        GlobalSearchScope scope = new ProjectAndLibrariesScope(place.project, true);

        // Add all classes.  TODO: This includes local classes, but probably shouldn't
        Map<PsiElement,Item> fake_globals = new HashMap<PsiElement,Item>();
        Converter env = new Converter(place,fake_globals,globals);
        for (String name : classnames) {

          // keep IDE responsive
          Utility.processEvents();

          for (PsiClass cls : cache.getClassesByName(name, scope))
            if (!place.isInaccessible(cls, true))
              env.addClass(cls, true, true);
        }

        logger.info("making global_env with " + globals.size() + " items.");

        // update global_env
        global_env = Tarski.environment(globals.values());

        logger.info("global_env ready.");

        globals_ready = true;

      }

      return globals;
    }
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarski engine to look up possible names
   */
  public Env getJavaEnvironment() {

    Map<PsiElement, Item> globals = getGlobals();
    Map<PsiElement, Item> locals = new HashMap<PsiElement, Item>();
    Map<Item, Integer> scopeItems = new HashMap<Item, Integer>();
    Converter env = new Converter(place,globals,locals);

    logger.info("adding local items...");

    // register locally visible items (each item will register things it contains, inherits from, etc.)
    for (ShadowElement<PsiPackage> spkg : packages) {
      final PsiPackage pkg = spkg.e;
      Item ipkg = env.addContainer(pkg);
      scopeItems.put(ipkg,spkg.shadowingPriority);
    }

    // then, register classes (we need those as containing elements in the other things)
    // classes may be contained in classes, so partial-order the list first
    for (ShadowElement<PsiClass> scls : classes) {
      final PsiClass cls = scls.e;
      // TODO: get type parameters etc
      // add private/protected stuff that's not already visible
      Item icls = env.addClass(cls, true, false);
      scopeItems.put(icls,scls.shadowingPriority);
    }

    // register methods (also register types used in this method)
    for (ShadowElement<PsiMethod> smethod : methods) {
      final PsiMethod method = smethod.e;
      Item imethod = env.addMethod(method);
      scopeItems.put(imethod,smethod.shadowingPriority);
    }

    // then, register objects which have types (enum constants, variables, parameters, fields), and their types
    for (ShadowElement<PsiVariable> svar : variables) {
      final PsiVariable var = svar.e;
      if (var instanceof PsiField) {
        Item ivar = env.addField((PsiField) var);
        scopeItems.put(ivar,svar.shadowingPriority);
      } else {
        assert !globals.containsKey(var);
        assert !locals.containsKey(var);
        Type t = env.convertType(var.getType());
        boolean isFinal = var.hasModifierProperty(PsiModifier.FINAL);
        Item i = var instanceof PsiParameter     ? new ParameterItem(var.getName(),t,isFinal)
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

    List<Item> local_items = new ArrayList<Item>();
    local_items.addAll(locals.values());

    // find out which element we are inside (method, class or interface, or package)
    PlaceItem placeItem = null;
    boolean inside_continuable = false;
    boolean inside_breakable = false;
    List<String> labels = new SmartList<String>();
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

      // add special "this" items this for each class we're inside of, with same shadowing priority as the class itself
      if (place instanceof PsiClass && !((PsiClass) place).isInterface()) { // don't make this for interfaces
        assert locals.containsKey(place) || globals.containsKey(place);
        ClassItem c = (ClassItem)env.addClass((PsiClass)place, false, false);
        assert scopeItems.containsKey(c);
        int p = scopeItems.get(c);
        ThisItem ti = new ThisItem(c);
        local_items.add(ti);
        scopeItems.put(ti,p);
      }

      if (place instanceof PsiMethod || place instanceof PsiClass || place instanceof PsiPackage) {
        if (placeItem == null) {
          assert globals.containsKey(place) || locals.containsKey(place);
          if (globals.containsKey(place))
            placeItem = (PlaceItem)globals.get(place);
          else if (locals.containsKey(place))
            placeItem = (PlaceItem)locals.get(place);
        }
      } else if (place instanceof PsiJavaFile) {
        PsiPackage pkg = this.place.getPackage((PsiJavaFile) place);
        if (pkg == null) {
          // probably we're top-level in a file without package statement, use LocalPackageItem
          if (placeItem == null)
            placeItem = Tarski.localPkg();
        } else {
          if (placeItem == null) {
            assert locals.containsKey(pkg) || globals.containsKey(pkg);
            placeItem = (PlaceItem)env.addContainer(pkg);
          }
        }
        break;
      }
      place = place.getParent();
    }
    assert placeItem != null;

    logger.info("environment (" + local_items.size() + " local items) taken inside " + placeItem + ", making env");

    /*
    for (NamedItem item: localItems.keySet()) {
      if (item.qualifiedName().startsWith("java.lang."))
        continue;
      logger.debug("  " + item);
    }

    for (NamedItem item : items) {
      logger.debug("  " + item + (localItems.containsKey(item) ? " scope level " + localItems.get(item).toString() : " not in scope."));
    }
    */

    Env tenv = Tarski.add_environment(global_env, local_items, scopeItems)
                     .move(placeItem, inside_breakable, inside_continuable, JavaConversions.asScalaBuffer(labels).toList());

    logger.info("done");

    return tenv;

  }

  private String qualifiedName(PsiElement elem) {
    if (elem instanceof PsiQualifiedNamedElement)
      return ((PsiQualifiedNamedElement) elem).getQualifiedName();
    else if (elem instanceof PsiMethod) {
      PsiClass cls = ((PsiMethod) elem).getContainingClass();
      assert cls != null;
      return cls.getQualifiedName() + '.' + ((PsiMethod) elem).getName();
    } else if (elem instanceof PsiEnumConstant) {
      PsiClass cls = ((PsiEnumConstant) elem).getContainingClass();
      assert cls != null;
      return cls.getQualifiedName() + '.' + ((PsiEnumConstant) elem).getName();
    } else if (elem instanceof PsiField) {
      PsiClass cls = ((PsiField) elem).getContainingClass();
      assert cls != null;
      return cls.getQualifiedName() + '.' + ((PsiField) elem).getName();
    }

    logger.error("Can't compute qualified name of " + elem);
    return null;
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

    // stop once we leave the file
    if (currentFileContext == null)
      return false;

    // if we are in static scope, a class member has to be declared static for us to see it
    if (element instanceof PsiField || element instanceof PsiMethod) {
      if (inStaticScope && !((PsiMember)element).hasModifierProperty(PsiModifier.STATIC))
        return true;
    }

    if (honorPrivate && place.isInaccessible((PsiModifierListOwner)element, false))
      return true;

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
