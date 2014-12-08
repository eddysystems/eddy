package com.eddysystems.eddy;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.impl.ApplicationImpl;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.util.DispatchThreadProgressWindow;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiClassReferenceType;
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
import scala.collection.JavaConversions;
import tarski.Environment.Env;
import tarski.Items.*;
import tarski.Makers.ClassItemMaker;
import tarski.Makers.TypeVarMaker;
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

  private final @NotNull
  Project project;

  private final @NotNull
  Logger logger = Logger.getInstance(getClass());

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
  static final Object global_envitems_lock = new Object();
  static boolean global_envitems_ready = false;
  static Map<PsiElement, Item> global_envitems = null;
  static Env global_env = null;

  // things that are in scope (not all these are accessible! things may be private, or not static while we are)
  private final List<ShadowElement<PsiPackage>> packages = new SmartList<ShadowElement<PsiPackage>>();
  private final List<ShadowElement<PsiClass>> classes = new SmartList<ShadowElement<PsiClass>>();
  private final List<ShadowElement<PsiVariable>> variables = new SmartList<ShadowElement<PsiVariable>>();
  private final List<ShadowElement<PsiMethod>> methods = new SmartList<ShadowElement<PsiMethod>>();

  private final PsiElement place;

  // used during walking
  private int currentLevel = 0;
  private boolean inStaticScope = false;
  private PsiElement currentFileContext;
  private boolean honorPrivate;

  // setup only for statics initialization -- the result of this cannot be used
  private EnvironmentProcessor(@NotNull Project project) {
    this.project = project;
    this.place = null;

    getGlobalEnvItems();
  }

  static public void initGlobalEnvironment(@NotNull Project project) {
    new EnvironmentProcessor(project);
  }

  public EnvironmentProcessor(@NotNull Project project, PsiElement place, boolean honorPrivate) {
    this.project = project;
    this.place = place;
    this.honorPrivate = honorPrivate;

    // this is set to null when we go to java.lang
    this.currentFileContext = place;

    logger.setLevel(Level.DEBUG);

    PsiScopesUtil.treeWalkUp(this, place, this.place.getContainingFile());

    // TODO: find import statements and add those to scope
  }

  private @Nullable PsiPackage getPackage(@NotNull PsiJavaFile file) {
    return JavaPsiFacade.getInstance(project).findPackage(file.getPackageName());
  }

  private PsiElement containing(PsiElement elem) {
    PsiElement parent = elem.getParent();
    if (parent instanceof PsiJavaFile) {
      return getPackage((PsiJavaFile) parent);
    } else if (parent instanceof PsiClass) {
      return parent;
    } else if (parent instanceof PsiDeclarationStatement || // local variable
              (parent instanceof PsiForeachStatement) || (parent instanceof PsiForStatement) || // declaration in for loop
              (parent instanceof PsiParameterList)) { // parameter to callable
      while (!(parent instanceof PsiMethod)) {
        logger.debug("walking up to find containing method for local class " + elem + ": " + parent);
        parent = parent.getParent();
      }
      return parent;
    } else if (parent instanceof PsiTypeParameterList) {
      assert elem instanceof PsiTypeParameter;
      return ((PsiTypeParameter) elem).getOwner();
    }
    throw new RuntimeException("unexpected container of " + elem + ": " + parent);
  }

  // the add* methods look up in global_ and local_ envitems, but add only to local_envitems.

  private Item addContainer(Map<PsiElement,Item> global_envitems, Map<PsiElement,Item> local_envitems, PsiElement elem) {
    if (global_envitems.containsKey(elem))
      return global_envitems.get(elem);
    if (local_envitems.containsKey(elem))
      return local_envitems.get(elem);

    // local classes
    if (elem instanceof PsiMethod)
      return addMethod(global_envitems, local_envitems, (PsiMethod) elem);
    if (elem instanceof PsiClass)
      return addClass(global_envitems, local_envitems, (PsiClass) elem, false, false);
    else if (elem instanceof PsiPackage) {
      PsiPackage pkg = (PsiPackage)elem;
      PackageItem pitem = new PackageItem(pkg.getName(), qualifiedName(pkg));
      Item base = Tarski.baseLookupJava(pitem);
      if (base != null)
        pitem = (PackageItem)base;
      local_envitems.put(pkg, pitem);
      return pitem;
    }
    throw new RuntimeException("weird container "+elem);
  }

  private TypeVar addTypeParam(Map<PsiElement,Item> global_envitems, Map<PsiElement,Item> local_envitems, PsiTypeParameter p) {
    if (global_envitems.containsKey(p))
      return (TypeVar)global_envitems.get(p);
    if (local_envitems.containsKey(p))
      return (TypeVar)local_envitems.get(p);

    // Add maker here to break recursion
    TypeVarMaker ti = new TypeVarMaker(p.getName());
    local_envitems.put(p,ti);

    PsiClassType[] extended = p.getExtendsList().getReferencedTypes();
    List<ClassType> etypes = new SmartList<ClassType>();
    for (PsiClassType e : extended) {
      etypes.add((ClassType)convertType(global_envitems, local_envitems, e));
    }

    ti.set(JavaConversions.asScalaBuffer(etypes).toList());
    return ti;
  }

  private TypeItem addClass(Map<PsiElement,Item> global_envitems, Map<PsiElement,Item> local_envitems, PsiClass cls, boolean recurse, boolean noProtected) {
    if (global_envitems.containsKey(cls))
      return (TypeItem)global_envitems.get(cls);
    if (local_envitems.containsKey(cls))
      return (TypeItem)local_envitems.get(cls);

    if (cls instanceof PsiTypeParameter)
      return addTypeParam(global_envitems, local_envitems, (PsiTypeParameter) cls);

    // java.lang.Object is special
    if (cls.getQualifiedName() != null && cls.getQualifiedName().equals("java.lang.Object")) {
      local_envitems.put(cls, ObjectItem$.MODULE$);
      return ObjectItem$.MODULE$;
    }

    PsiElement celem = containing(cls);
    ParentItem container = celem != null ? (ParentItem) addContainer(global_envitems, local_envitems, celem) : Tarski.localPkg();

    // Type parameters
    ArrayList<TypeVar> j_params = new ArrayList<TypeVar>();
    for (PsiTypeParameter tp: cls.getTypeParameters()) {
      j_params.add(addTypeParam(global_envitems, local_envitems, tp));
    }
    scala.collection.immutable.List<TypeVar> params = JavaConversions.asScalaBuffer(j_params).toList();

    // maybe we just added ourselves by adding the container or the type parameters (we only add to local_envitems
    if (local_envitems.containsKey(cls))
      return (TypeItem)local_envitems.get(cls);

    ClassItemMaker ci = new ClassItemMaker(cls.getName(), container, params, !cls.isInterface(), cls.isEnum(),
                                           cls.hasModifierProperty(PsiModifier.FINAL));
    Item ciBase = Tarski.baseLookupJava(ci);
    if (ciBase != null) {
      local_envitems.put(cls,ciBase);
      return (TypeItem)ciBase;
    }
    local_envitems.put(cls,ci);

    // Base
    // TODO: Handle generics
    PsiClass scls = cls.getSuperClass();
    assert scls != null;
    // TODO: get actual type arguments for scls and make generic class
    ClassType base = (ClassType) addClass(global_envitems, local_envitems, scls, false, false).raw();

    // Interfaces
    ArrayList<ClassType> j_interfaces = new ArrayList<ClassType>();
    for (PsiClass i : cls.getInterfaces()) {
      // TODO: Handle generics
      j_interfaces.add(((ClassItem) addClass(global_envitems, local_envitems, i, false, false)).raw());
    }
    scala.collection.immutable.List<ClassType> interfaces = JavaConversions.asScalaBuffer(j_interfaces).toList();

    ci.set(base, interfaces);

    // add subthings recursively
    if (recurse) {
      for (PsiField f : cls.getFields()) {
        if (!isInaccessible(f, noProtected))
          addField(global_envitems, local_envitems, f);
      }
      for (PsiMethod m : cls.getMethods()) {
        if (!isInaccessible(m, noProtected))
          addMethod(global_envitems, local_envitems, m);
      }
      for (PsiMethod m : cls.getConstructors()) {
        if (!isInaccessible(m, noProtected))
          addMethod(global_envitems, local_envitems, m);
      }
      for (PsiClass c : cls.getInnerClasses()) {
        if (!isInaccessible(c, noProtected))
          addClass(global_envitems, local_envitems, c, true, noProtected);
      }
    }

    return ci;
  }

  private CallableItem addMethod(Map<PsiElement,Item> global_envitems, Map<PsiElement,Item> local_envitems, PsiMethod method) {
    if (global_envitems.containsKey(method))
      return (CallableItem)global_envitems.get(method);
    if (local_envitems.containsKey(method))
      return (CallableItem)local_envitems.get(method);

    // get type parameters
    List<TypeVar> jtparams = new ArrayList<TypeVar>();
    for (PsiTypeParameter tp : method.getTypeParameters()) {
      jtparams.add(addTypeParam(global_envitems, local_envitems, tp));
    }
    scala.collection.immutable.List<TypeVar> tparams = scala.collection.JavaConversions.asScalaBuffer(jtparams).toList();

    // get argument types
    List<Type> jparams = new SmartList<Type>();
    for (PsiParameter p : method.getParameterList().getParameters())
      jparams.add(convertType(global_envitems, local_envitems, p.getType()));
    scala.collection.immutable.List<Type> params = scala.collection.JavaConversions.asScalaBuffer(jparams).toList();

    // get class
    PsiClass cls = method.getContainingClass();
    assert cls != null;
    ClassItem clsitem = (ClassItem)addClass(global_envitems, local_envitems, cls, false, false);

    CallableItem mitem;
    if (method.isConstructor()) {
      // TODO: varargs
      mitem = new ConstructorItem(clsitem, tparams, params);
      Item base = Tarski.baseLookupJava(mitem);
      if (base != null)
        mitem = (ConstructorItem)base;
    } else {
      // TODO: varargs
      Type rtype = convertType(global_envitems, local_envitems, method.getReturnType());

      if (method.hasModifierProperty(PsiModifier.STATIC))
        mitem = new StaticMethodItem(method.getName(), clsitem, tparams, rtype, params);
      else
        mitem = new MethodItem(method.getName(), clsitem, tparams, rtype, params);
    }

    local_envitems.put(method, mitem);
    return mitem;
  }

  private Type convertType(Map<PsiElement, Item> global_envitems, Map<PsiElement, Item> local_envitems, PsiType t) {
    // TODO: Handle modifiers
    if (t instanceof PsiArrayType)
      return new ArrayType(convertType(global_envitems, local_envitems, ((PsiArrayType)t).getComponentType()));

    if (t instanceof PsiWildcardType) {
      // TODO: need proper wildcard expressions
      PsiType bound = ((PsiWildcardType) t).getBound();
      if (bound != null)
        return convertType(global_envitems, local_envitems, bound);
      else
        return ObjectType$.MODULE$;
    }

    // classes are not types in IntelliJ's version of the world, so we have to look up this class in envitems
    if (t instanceof PsiClassType) {
      PsiClass tcls = ((PsiClassType)t).resolve();
      if (tcls == null) {
        // ClassType cannot be resolved to a Class (probably because its class file is missing or the code is incomplete)
        String name = ((PsiClassType)t).getClassName();

        logger.warn("cannot resolve type " + t);

        String qname = "";
        boolean isfinal = false;
        List<TypeArg> jargs = new SmartList<TypeArg>();

        if (t instanceof PsiClassReferenceType) {
          qname = ((PsiClassReferenceType)t).getReference().getQualifiedName();
          if (t instanceof PsiModifierListOwner)
            isfinal = ((PsiModifierListOwner) t).hasModifierProperty(PsiModifier.FINAL);
          // TODO: pass on type parameters
        }

        scala.collection.immutable.List<TypeArg> args = scala.collection.JavaConversions.asScalaBuffer(jargs).toList();
        return new UnresolvedClassItem(name, qname.substring(qname.lastIndexOf('.')+1), args, isfinal).generic();
      } else if (tcls instanceof PsiTypeParameter) {
        return addTypeParam(global_envitems, local_envitems, (PsiTypeParameter)tcls);
      } else {
        List<TypeArg> jparams = new SmartList<TypeArg>();
        for (PsiType tp : ((PsiClassType)t).getParameters()) {
          jparams.add((TypeArg)convertType(global_envitems, local_envitems, tp));
        }
        scala.collection.immutable.List<TypeArg> params = scala.collection.JavaConversions.asScalaBuffer(jparams).toList();

        //logger.debug("converting class " + t + " with type parameters " + params.mkString("[",",","]"));

        ClassItem item = (ClassItem) addClass(global_envitems, local_envitems, tcls, false, false);

        //logger.debug("  item: " + item + ": params " + item.tparams().mkString("[",",","]"));

        assert params.size() == ((PsiClassType)t).getParameterCount();

        if (item.arity() > 0 && params.isEmpty()) {
          // This happens. For instance in java.lang.SecurityManager.getClassContext()
          return item.raw();
        } else {
          Item container = addContainer(global_envitems, local_envitems, containing(tcls));
          assert container instanceof ParentItem;
          return item.generic(params, ((ParentItem)container).inside());
        }
      }
    }

    if (t == PsiType.BOOLEAN) return BooleanType$.MODULE$;
    if (t == PsiType.INT)     return IntType$.MODULE$;
    if (t == PsiType.BYTE)    return ByteType$.MODULE$;
    if (t == PsiType.CHAR)    return CharType$.MODULE$;
    if (t == PsiType.FLOAT)   return FloatType$.MODULE$;
    if (t == PsiType.DOUBLE)  return DoubleType$.MODULE$;
    if (t == PsiType.LONG)    return LongType$.MODULE$;
    if (t == PsiType.SHORT)   return ShortType$.MODULE$;
    if (t == PsiType.VOID)    return VoidType$.MODULE$;

    throw new NotImplementedError("Unknown type: " + t.getCanonicalText() + " type " + t.getClass().getCanonicalName());
  }

  private Value addField(Map<PsiElement,Item> global_envitems, Map<PsiElement,Item> local_envitems, PsiField f) {
    if (global_envitems.containsKey(f))
      return (Value)global_envitems.get(f);
    if (local_envitems.containsKey(f))
      return (Value)local_envitems.get(f);

    PsiClass cls = f.getContainingClass();
    assert cls != null;
    Type t = convertType(global_envitems, local_envitems, f.getType());
    ClassItem c = (ClassItem)addClass(global_envitems, local_envitems, cls, false, false);
    boolean isFinal = f.hasModifierProperty(PsiModifier.FINAL);
    Value v =               f instanceof PsiEnumConstant ? new EnumConstantItem(f.getName(),c) :
              (f.hasModifierProperty(PsiModifier.STATIC) ? new StaticFieldItem(f.getName(),t,c,isFinal)
                                                         : new FieldItem(f.getName(),t,c,isFinal));
    local_envitems.put(f, v);
    return v;
  }

  private Map<PsiElement,Item> getGlobalEnvItems() {
    if (global_envitems_ready) {
      return global_envitems;
    } else {

      synchronized (global_envitems_lock) {

        if (global_envitems == null) {
          // get all classes from IntelliJ
          global_envitems = new HashMap<PsiElement, Item>();
        }

        logger.info("making global_envitems (" + global_envitems.size() + " items already there)");

        PsiShortNamesCache cache = PsiShortNamesCache.getInstance(project);
        String[] classnames = cache.getAllClassNames();
        GlobalSearchScope scope = new ProjectAndLibrariesScope(project, true);

        // Add all classes.  TODO: This includes local classes, but probably shouldn't
        Map<PsiElement,Item> fake_globals = new HashMap<PsiElement,Item>();
        for (String name : classnames) {

          // keep IDE responsive
          Utility.processEvents();

          for (PsiClass cls : cache.getClassesByName(name, scope))
            if (!isInaccessible(cls, true))
              addClass(fake_globals, global_envitems, cls, true, true);
        }

        logger.info("making global_env with " + global_envitems.size() + " items.");

        // update global_env
        global_env = Tarski.environment(global_envitems.values());

        logger.info("global_env ready.");

        global_envitems_ready = true;

      }

      return global_envitems;
    }
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarski engine to look up possible names
   */
  public Env getJavaEnvironment() {

    Map<PsiElement, Item> global_envitems = getGlobalEnvItems();
    Map<PsiElement, Item> local_envitems = new HashMap<PsiElement, Item>();
    Map<Item, Integer> scopeItems = new HashMap<Item, Integer>();

    logger.info("adding local items...");

    // register locally visible items (each item will register things it contains, inherits from, etc.)
    for (ShadowElement<PsiPackage> spkg : packages) {
      final PsiPackage pkg = spkg.e;
      Item ipkg = addContainer(global_envitems, local_envitems, pkg);
      scopeItems.put(ipkg,spkg.shadowingPriority);
    }

    // then, register classes (we need those as containing elements in the other things)
    // classes may be contained in classes, so partial-order the list first
    for (ShadowElement<PsiClass> scls : classes) {
      final PsiClass cls = scls.e;
      // TODO: get type parameters etc
      // add private/protected stuff that's not already visible
      Item icls = addClass(global_envitems, local_envitems, cls, true, false);
      scopeItems.put(icls,scls.shadowingPriority);
    }

    // register methods (also register types used in this method)
    for (ShadowElement<PsiMethod> smethod : methods) {
      final PsiMethod method = smethod.e;
      Item imethod = addMethod(global_envitems, local_envitems, method);
      scopeItems.put(imethod,smethod.shadowingPriority);
    }

    // then, register objects which have types (enum constants, variables, parameters, fields), and their types
    for (ShadowElement<PsiVariable> svar : variables) {
      final PsiVariable var = svar.e;
      if (var instanceof PsiField) {
        Item ivar = addField(global_envitems, local_envitems, (PsiField) var);
        scopeItems.put(ivar,svar.shadowingPriority);
      } else {
        assert !global_envitems.containsKey(var);
        assert !local_envitems.containsKey(var);
        Type t = convertType(global_envitems, local_envitems, var.getType());
        boolean isFinal = var.hasModifierProperty(PsiModifier.FINAL);
        Item i = var instanceof PsiParameter     ? new ParameterItem(var.getName(),t,isFinal)
               : var instanceof PsiLocalVariable ? new LocalVariableItem(var.getName(),t,isFinal)
               : null;
        if (i == null)
          throw new scala.NotImplementedError("Unknown variable: " + var);

        // actually add to envitems map
        local_envitems.put(var, i);
        scopeItems.put(i,svar.shadowingPriority);
      }
    }

    logger.info("added " + local_envitems.size() + " local envitems");

    List<Item> local_items = new ArrayList<Item>();
    local_items.addAll(local_envitems.values());

    // find out which element we are inside (method, class or interface, or package)
    PlaceItem placeItem = null;
    boolean inside_continuable = false;
    boolean inside_breakable = false;
    List<String> labels = new SmartList<String>();
    // walk straight up until we see a method, class, or package
    PsiElement place = this.place;
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
        assert local_envitems.containsKey(place) || global_envitems.containsKey(place);
        ClassItem c = (ClassItem)addClass(global_envitems, local_envitems, (PsiClass)place, false, false);
        assert scopeItems.containsKey(c);
        int p = scopeItems.get(c);
        ThisItem ti = new ThisItem(c);
        local_items.add(ti);
        scopeItems.put(ti,p);
      }

      if (place instanceof PsiMethod || place instanceof PsiClass || place instanceof PsiPackage) {
        if (placeItem == null) {
          assert global_envitems.containsKey(place) || local_envitems.containsKey(place);
          if (global_envitems.containsKey(place))
            placeItem = (PlaceItem)global_envitems.get(place);
          else if (local_envitems.containsKey(place))
            placeItem = (PlaceItem)local_envitems.get(place);
        }
      } else if (place instanceof PsiJavaFile) {
        PsiPackage pkg = getPackage((PsiJavaFile)place);
        if (pkg == null) {
          // probably we're top-level in a file without package statement, use LocalPackageItem
          if (placeItem == null)
            placeItem = Tarski.localPkg();
        } else {
          if (placeItem == null) {
            assert local_envitems.containsKey(pkg) || global_envitems.containsKey(pkg);
            placeItem = (PlaceItem)addContainer(global_envitems, local_envitems, pkg);
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

    Env env = Tarski.add_environment(global_env, local_items, scopeItems)
                    .move(placeItem, inside_breakable, inside_continuable, JavaConversions.asScalaBuffer(labels).toList());

    logger.info("done");

    return env;

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

  /**
   * Check if a thing (member or class) is private or protected and therefore not accessible
   * @param element The thing to check whether it's inaccessible because it may be private or protected
   * @param noProtected Consider all protected, private, or package local items inaccessible
   * @return whether the element is private or protected and not accessible because of it
   */
  private boolean isInaccessible(PsiModifierListOwner element, boolean noProtected) {

    PsiElement container = containing(element);

    if (container instanceof PsiPackage && !element.hasModifierProperty(PsiModifier.PUBLIC)) {
      if (noProtected)
        return true;

      PsiJavaFile file = PsiTreeUtil.getParentOfType(place, PsiJavaFile.class, false);
      if (file != null && container != getPackage(file))
        return true;

      return false;
    }

    if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
      if (noProtected)
        return true;

      if (container instanceof PsiClass) {
        // if the member is private we can only see it if place is contained in a class in which member is declared.
        PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
        while (containingPlaceClass != null) {
          if (container == containingPlaceClass) {
            break;
          }
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class);
        }
        if (containingPlaceClass == null) {
          return true;
        }
      }
    }

    if (element.hasModifierProperty(PsiModifier.PROTECTED)) {
      if (noProtected)
        return true;

      // if the member is protected we can only see it if place is contained in a subclass of the containingClass
      if (container instanceof PsiClass) {
        PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
        while (containingPlaceClass != null) {
          if (containingPlaceClass.isInheritor((PsiClass)container, true))
            break;
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class);
        }
        if (containingPlaceClass == null) {
          return true;
        }
      }
    }

    return false;
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

    if (honorPrivate) {
      if (isInaccessible((PsiModifierListOwner)element, false)) {
        return true;
      }
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
