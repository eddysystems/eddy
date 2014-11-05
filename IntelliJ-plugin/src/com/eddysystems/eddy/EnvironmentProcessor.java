package com.eddysystems.eddy;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import scala.collection.JavaConversions;
import tarski.Environment.Env;
import tarski.Items.*;
import tarski.Types.*;
import tarski.Tarski;

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

  // things that are in scope (not all these are accessible! things may be private, or not static while we are)
  private final List<PsiPackage> packages = new SmartList<PsiPackage>();
  private final List<PsiClass> classes = new SmartList<PsiClass>();
  private final List<PsiVariable> variables = new SmartList<PsiVariable>();
  private final List<PsiMethod> methods = new SmartList<PsiMethod>();

  private final PsiElement place;

  // used during walking
  private boolean inStaticScope = false;
  private PsiElement currentFileContext;
  private boolean honorPrivate;

  public EnvironmentProcessor(@NotNull Project project, PsiElement place, boolean honorPrivate) {
    this.project = project;
    this.place = place;
    this.honorPrivate = honorPrivate;
    PsiScopesUtil.treeWalkUp(this, place, null);
  }

  private Type makeArray(Type t, int dims) {
    assert dims >= 1;
    while (dims-- != 0)
      t = new ArrayType(t);
    return t;
  }

  private PsiElement containing(PsiElement cls) {
    PsiElement parent = cls.getParent();
    if (parent instanceof PsiJavaFile) {
      return JavaPsiFacade.getInstance(project).findPackage(((PsiJavaFile) parent).getPackageName());
    } else if (parent instanceof PsiClass) {
      return parent;
    }
    logger.error("parent of " + cls + " = " + parent);
    throw new RuntimeException("unexpected parent of " + cls + " = " + parent);
  }

  private NamedItem addContainerToEnvMap(Map<PsiElement, NamedItem> envitems, PsiElement elem) {
    if (elem instanceof PsiClass)
      return addClassToEnvMap(envitems, (PsiClass)elem);
    else if (elem instanceof PsiPackage) {
      PsiPackage pkg = (PsiPackage)elem;
      if (!envitems.containsKey(pkg)) {
        PackageItem pitem = new PackageItem(pkg.getName(), qualifiedName(pkg));
        envitems.put(pkg, pitem);
        return pitem;
      } else
        return envitems.get(pkg);
    }
    return null;
  }

  private TypeItem addClassToEnvMap(Map<PsiElement, NamedItem> envitems, PsiClass cls) {
    if (cls == null)
      return null;
    if (envitems.containsKey(cls))
      return (TypeItem)envitems.get(cls);

    // Base
    // TODO: Handle generics
    SimpleClassType base = new SimpleClassType((ClassItem)addClassToEnvMap(envitems, cls.getSuperClass()));

    // Type parameters
    // TODO: Handle generics
    ArrayList<TypeParamItem> j_params = new ArrayList<TypeParamItem>();
    scala.collection.immutable.List<TypeParamItem> params = JavaConversions.asScalaBuffer(j_params).toList();

    // Interfaces
    ArrayList<InterfaceType> j_interfaces = new ArrayList<InterfaceType>();
    for (PsiClass i : cls.getInterfaces()) {
      // TODO: Handle generics
      j_interfaces.add(new SimpleInterfaceType((InterfaceItem)addClassToEnvMap(envitems,i)));
    }
    scala.collection.immutable.List<InterfaceType> interfaces = JavaConversions.asScalaBuffer(j_interfaces).toList();

    NamedItem container = addContainerToEnvMap(envitems,containing(cls));
    TypeItem item = cls.isInterface() ? new InterfaceItem(cls.getName(),container,params,interfaces)
                  : cls.isEnum()      ? new EnumItem(cls.getName(),container,interfaces)
                                      : new NormalClassItem(cls.getName(),container,params,base,interfaces);
    envitems.put(cls,item);
    return item;
  }

  private CallableItem addMethodToEnvMap(Map<PsiElement,NamedItem> envitems, PsiMethod method) {
    // get argument types
    List<Type> params = new SmartList<Type>();
    for (PsiParameter p : method.getParameterList().getParameters())
      params.add(convertType(envitems,p.getType()));

    // get class
    PsiClass cls = method.getContainingClass();
    assert cls != null;
    if (!envitems.containsKey(cls)) {
      addClassToEnvMap(envitems, cls);
    }
    TypeItem clsitem = (TypeItem)envitems.get(cls);

    CallableItem mitem;

    if (method.isConstructor()) {
      assert clsitem instanceof ClassItem;
      // TODO: varargs
      // TODO: get type parameters
      // TODO: what to do with parameters depending on type parameters and bounded types and such?
      mitem = new ConstructorItem((ClassItem)clsitem, scala.collection.JavaConversions.asScalaBuffer(params).toList());
    } else {
      // TODO: varargs
      // TODO: get type parameters
      // TODO: what to do with parameters depending on type parameters and bounded types and such?
      Type rtype = convertType(envitems, method.getReturnType());

      if (method.hasModifierProperty(PsiModifier.STATIC))
        mitem = new StaticMethodItem(method.getName(), clsitem, rtype, scala.collection.JavaConversions.asScalaBuffer(params).toList());
      else
        mitem = new MethodItem(method.getName(), clsitem, rtype, scala.collection.JavaConversions.asScalaBuffer(params).toList());
    }

    envitems.put(method, mitem);
    return mitem;
  }

  private Type convertType(Map<PsiElement,NamedItem> envitems, PsiType t) {
    // TODO: Handle modifiers
    if (t instanceof PsiArrayType)
      return new ArrayType(convertType(envitems,((PsiArrayType)t).getComponentType()));

    // classes are not types in IntelliJ's version of the world, so we have to look up this class in envitems
    if (t instanceof PsiClassType) {
      PsiClass tcls = ((PsiClassType)t).resolve();
      if (tcls == null) {
        String name = ((PsiClassType)t).getClassName();
        return new tarski.Types.ErrorType(name);
      } else if (tcls instanceof PsiTypeParameter) {
        // TODO: this should resolve to an existing type parameter (stored in the method's or class's type parameters array)
        throw new NotImplementedException("type parameters");
        // return new TypeParamType(...);
      } else {
        // TODO: Handle generics
        return new SimpleClassType((ClassItem)addClassToEnvMap(envitems,tcls));
      }
    }
    if (t == PsiType.BOOLEAN) return tarski.Types.BooleanType$.MODULE$;
    if (t == PsiType.INT)     return tarski.Types.IntType$.MODULE$;
    if (t == PsiType.BYTE)    return tarski.Types.ByteType$.MODULE$;
    if (t == PsiType.CHAR)    return tarski.Types.CharType$.MODULE$;
    if (t == PsiType.FLOAT)   return tarski.Types.FloatType$.MODULE$;
    if (t == PsiType.DOUBLE)  return tarski.Types.DoubleType$.MODULE$;
    if (t == PsiType.LONG)    return tarski.Types.LongType$.MODULE$;
    if (t == PsiType.SHORT)   return tarski.Types.ShortType$.MODULE$;
    if (t == PsiType.VOID)    return tarski.Types.VoidType$.MODULE$;
    throw new RuntimeException("Unknown type: " + t.getCanonicalText());
  }

  private Value addFieldToEnvMap(Map<PsiElement, NamedItem> envitems, PsiField f) {
    if (envitems.containsKey(f))
      return (Value)envitems.get(f);

    PsiClass cls = f.getContainingClass();
    assert cls != null;
    Type t = convertType(envitems,f.getType());
    ClassItem c = (ClassItem)envitems.get(cls);
    Value v = f.hasModifierProperty(PsiModifier.STATIC) ? new StaticFieldItem(f.getName(),t,c)
                                                        : new FieldItem(f.getName(),t,c);
    envitems.put(f, v);
    return v;
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarksi engine to look up possible names
   */
  public Env getJavaEnvironment() {
    Map<PsiElement, NamedItem> envitems = new HashMap<PsiElement, NamedItem>();

    // first, register packages (we may need those as containing elements in classes)
    for (PsiPackage pkg : packages) {
      addContainerToEnvMap(envitems, pkg);

      // TODO: register everything that is below this package (some of which may already be in the env map)
    }

    // then, register classes (we need those as containing elements in the other things)
    // classes may be contained in classes, so partial-order the list first
    for (PsiClass cls : classes) {
      // TODO: get type parameters etc
      addClassToEnvMap(envitems, cls);

      // TODO: register everything that is below this class (some of which may already be in the env map)
      for (PsiField f : cls.getFields()) {
        addFieldToEnvMap(envitems,f);
      }
      for (PsiMethod m : cls.getMethods()) {
        if (!envitems.containsKey(m))
          addMethodToEnvMap(envitems,m);
      }
      for (PsiMethod m : cls.getConstructors()) {
        if (!envitems.containsKey(m))
          addMethodToEnvMap(envitems,m);
      }
      for (PsiClass c : cls.getInnerClasses()) {
        if (!envitems.containsKey(c))
          addClassToEnvMap(envitems,c);
      }
    }

    // register methods (also register types used in this method)
    for (PsiMethod method : methods) {
      addMethodToEnvMap(envitems,method);
    }

    // then, register objects which have types (enum constants, variables, parameters, fields), and their types
    for (PsiVariable var : variables) {
      if (var instanceof PsiField)
        addFieldToEnvMap(envitems,(PsiField)var);
      else {
        Type t = convertType(envitems,var.getType());
        NamedItem i = var instanceof PsiParameter     ? new ParameterItem(var.getName(),t)
                    : var instanceof PsiEnumConstant  ? new EnumConstantItem(var.getName(),(EnumItem)((SimpleClassType)t).d())
                    : var instanceof PsiLocalVariable ? new LocalVariableItem(var.getName(),t)
                    : null;
        if (i == null)
          throw new NotImplementedException("Unknown variable");
      }
    }

    return Tarski.environment(envitems.values());
  }

  /**
   * Compute a name for any element type that matter to us
   */
  private String name(PsiElement elem) {
    if (elem instanceof PsiNamedElement) {
      return ((PsiNamedElement) elem).getName();
    } else {
      logger.error("Can't compute name of " + elem);
      return null;
    }
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
   * Compute the relative name of an object (the minimally qualified name needed to access it from here
   */
  private String relativeName(PsiElement elem) {
    // TODO
    return null;
  }

  /**
   * Check if a thing (member or class) is private or protected and therefore not accessible
   * @param element The thing to check whether it's inaccessible because it may be private or protected
   * @param containingClass The class containing the thing
   * @return whether the element is private or protected and not accessible because of it
   */
  private boolean isInaccessible(PsiModifierListOwner element, PsiClass containingClass) {
    if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
      // if the member is private we can only see it if place is contained in a class in which member is declared.
      PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
      while (containingPlaceClass != null) {
        if (containingClass == containingPlaceClass) {
          break;
        }
        containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class);
      }
      if (containingPlaceClass == null) {
        return true;
      }
    }

    if (element.hasModifierProperty(PsiModifier.PROTECTED)) {
      // if the member is protected we can only see it if place is contained in a subclass of the containingClass
      PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
      while (containingPlaceClass != null) {
        if (containingPlaceClass == containingClass)
          break;
        containingPlaceClass = containingPlaceClass.getSuperClass();
      }
      if (containingPlaceClass == null) {
        return true;
      }
    }

    return false;
  }

  /**
   * Check if a variable is visible in our place (not private member in a superclass)
   */
  public boolean isVisible(PsiVariable var) {
    if (var instanceof PsiMember) {
      PsiMember member = (PsiMember)var;
      return !isInaccessible(member, member.getContainingClass());
    } else
      return true; // parameters and local variables are always visible
  }

  /**
   * Check if a member (field or method) is visible in our place (not private in a superclass)
   */
  public boolean isVisible(PsiMember member) {
    return !isInaccessible(member, member.getContainingClass());
  }

  /**
   * Check if a class is visible in our place (not private)
   */
  public boolean isVisible(PsiClass clazz) {
    return !isInaccessible(clazz, clazz.getContainingClass());
  }

  @Override
  public boolean shouldProcess(DeclarationKind kind) {
    return kind == DeclarationKind.CLASS ||
      kind == DeclarationKind.FIELD ||
      kind == DeclarationKind.METHOD ||
      kind == DeclarationKind.VARIABLE ||
      kind == DeclarationKind.PACKAGE ||
      kind == DeclarationKind.ENUM_CONST;
  }

  @Override
  public boolean execute(@NotNull PsiElement element, ResolveState state) {

    // if we are in static scope, a class member has to be declared static for us to see it
    if (element instanceof PsiField || element instanceof PsiMethod) {
      if (inStaticScope && !((PsiMember)element).hasModifierProperty(PsiModifier.STATIC))
        return true;
    }

    if (honorPrivate) {
      PsiClass containing = null;
      if (element instanceof PsiMember)
        containing = ((PsiMember)element).getContainingClass();

      if (containing != null && isInaccessible((PsiModifierListOwner)element, containing)) {
        return true;
      }
    }

    if (element instanceof PsiClass) {
      classes.add((PsiClass)element);
    } else if (element instanceof PsiVariable) {
      variables.add((PsiVariable)element);
    } else if (element instanceof PsiMethod) {
      methods.add((PsiMethod)element);
    } else if (element instanceof PsiPackage) {
      packages.add((PsiPackage)element);
    }
    return true;
  }

  @Override
  public final void handleEvent(@NotNull Event event, Object associated){
    if (event == JavaScopeProcessorEvent.START_STATIC) {
      logger.debug("starting in static scope");
      inStaticScope = true;
    } else if (event == JavaScopeProcessorEvent.SET_CURRENT_FILE_CONTEXT)
      currentFileContext = (PsiElement)associated;
      logger.debug("switching file context: " + currentFileContext);
  }
}
