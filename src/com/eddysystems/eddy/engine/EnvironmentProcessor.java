package com.eddysystems.eddy.engine;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;
import tarski.Environment.PlaceInfo;
import tarski.Environment;
import tarski.Items.*;
import tarski.Types.ClassType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.eddysystems.eddy.engine.Utility.log;

/**
 * Extracts information about the environment at a given place in the code and makes it available in a format understood by tarski
 */
class EnvironmentProcessor extends BaseScopeProcessor implements ElementClassHint {
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

  // filled in fillLocalInfo
  public PlaceInfo placeInfo;
  public final List<Item> localItems = new ArrayList<Item>();
  public final Map<Item,Integer> scopeItems = new HashMap<Item,Integer>();
  final Map<PsiElement,Item> locals;
  final JavaEnvironment jenv;

  // add to locals what you find in scope. There may be several threads writing to locals at the same time.
  public EnvironmentProcessor(@NotNull Project project, @NotNull JavaEnvironment jenv, Map<PsiElement,Item> locals, @NotNull PsiElement place, int lastedit, boolean honorPrivate) {
    this.place = new Place(project,place);
    this.honorPrivate = honorPrivate;
    this.locals = locals;
    this.jenv = jenv;

    // this is set to null when we go to java.lang
    this.currentFileContext = place;

    // this walks up the PSI tree, but also processes import statements
    PsiScopesUtil.treeWalkUp(this, place, place.getContainingFile());
    fillLocalInfo(lastedit);
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarski engine to look up possible names, using jenv as a base
   */
  private void fillLocalInfo(int lastedit) {

    // local variables, parameters, type parameters, as well as protected/private things in scope
    final Converter env = new Converter(jenv,locals);

    log("getting local items...");

    // register locally visible packages
    for (ShadowElement<PsiPackage> spkg : packages) {
      final PsiPackage pkg = spkg.e;
      final Item ipkg = env.addContainer(pkg);
      scopeItems.put(ipkg,spkg.shadowingPriority);
    }

    // register classes
    for (ShadowElement<PsiClass> scls : classes) {
      final PsiClass cls = scls.e;
      final Item icls = env.addClass(cls, true);
      scopeItems.put(icls,scls.shadowingPriority);
    }

    // register methods
    for (ShadowElement<PsiMethod> smethod : methods) {
      final PsiMethod method = smethod.e;
      final Item imethod = env.addMethod(method);
      if (!(imethod instanceof ConstructorItem)) {
        scopeItems.put(imethod,smethod.shadowingPriority);
      }
    }

    // then, register values
    for (ShadowElement<PsiVariable> svar : variables) {
      final PsiVariable var = svar.e;
      if (var instanceof PsiField) {
        final Item ivar = env.addField((PsiField) var);
        scopeItems.put(ivar,svar.shadowingPriority);
      } else {
        // true local variables (parameters or local variables)
        final Item i = env.addLocal(var);
        scopeItems.put(i,svar.shadowingPriority);
      }
    }

    log("added " + locals.size() + " locals");

    synchronized(jenv) {
      // .values is undefined if it is modified during iteration.
      localItems.addAll(locals.values());
    }

    // find out which element we are inside (method, class or interface, or package)
    ParentItem placeItem = null;
    boolean inside_continuable = false;
    boolean inside_breakable = false;
    // walk straight up until we see a method, class, or package
    PsiElement place = this.place.place;
    while (place != null) {
      // scan the current method for labels, loops, and switch statements
      if (placeItem != null) {
        if (place instanceof PsiLabeledStatement) {
          final PsiLabeledStatement lab = (PsiLabeledStatement)place;
          final boolean continuable = !(lab instanceof PsiSwitchStatement);
          localItems.add(new Label(lab.getLabelIdentifier().getText(),continuable));
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
        final ClassItem c = (ClassItem)env.addClass((PsiClass)place, false);
        assert scopeItems.containsKey(c);
        final int p = scopeItems.get(c);
        final ThisItem ti = new ThisItem(c);
        localItems.add(ti);
        scopeItems.put(ti,p);

        final ClassType s = c.base();
        assert s.item().isClass();
        final SuperItem si = new SuperItem(s);
        localItems.add(si);
        scopeItems.put(si,p);
      }

      if (place instanceof PsiMethod || place instanceof PsiClass || place instanceof PsiPackage) {
        if (placeItem == null) {
          placeItem = (ParentItem)jenv.lookup(place, true);
          assert placeItem != null : "cannot find placeItem " + place + ", possibly in anonymous local class";
        }
      } else if (place instanceof PsiJavaFile) {
        final PsiPackage pkg = Place.getPackage((PsiJavaFile) place, env.project);
        if (pkg == null) {
          // probably we're top-level in a file without package statement, use LocalPackageItem
          if (placeItem == null)
            placeItem = LocalPkg$.MODULE$;
        } else {
          if (placeItem == null) {
            assert jenv.knows(pkg);
            placeItem = env.addContainer(pkg);
          }
        }
        break;
      }
      place = place.getParent();
    }
    assert placeItem != null;

    log("environment (" + localItems.size() + " local items) taken inside " + placeItem);

    placeInfo = Environment.PlaceInfoJava(placeItem, this.place.place, inside_breakable, inside_continuable, lastedit);
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

    if (honorPrivate && place.isInaccessible((PsiModifierListOwner)element)) {
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
      //log("starting static scope");
      inStaticScope = true;
    } else if (event == JavaScopeProcessorEvent.SET_CURRENT_FILE_CONTEXT) {
      currentFileContext = (PsiElement)associated;
      if (associated instanceof PsiAnonymousClass)
        classes.add(new ShadowElement<PsiClass>((PsiClass)associated,currentLevel));
      //log("switching context: " + currentFileContext);
    } else if (event == JavaScopeProcessorEvent.CHANGE_LEVEL) {
      currentLevel++;
      //log("change level to " + currentLevel + ", associated " + associated);
    }
  }
}
