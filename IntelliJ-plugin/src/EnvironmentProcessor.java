import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;

import tarski.Environment.*;
import tarski.Items.*;
import tarski.Tarski;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Extracts information about the environment at a given place in the code and makes it available in a format understood by tarski
 */
public class EnvironmentProcessor extends BaseScopeProcessor implements ElementClassHint {

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

  EnvironmentProcessor(PsiElement place, boolean honorPrivate) {
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

  private Type addTypeToEnvMap(Map<PsiElement, NamedItem> envitems, Map<PsiType, NamedItem> types, PsiType t) {
    if (!types.containsKey(t)) {
      // TODO: get modifiers

      // remove the array part
      PsiType inner = t.getDeepComponentType();

      Type env_inner = null;

      // classes are not types in IntelliJ's version of the world, so we have to look up this class in envitems
      if (inner instanceof PsiClassType) {
        PsiClass tcls = ((PsiClassType) inner).resolve();
        if (tcls == null) {
          return new ErrorType();
        } else {
          if (!envitems.containsKey(tcls)) {
            if (tcls.isInterface())
              envitems.put(tcls, new InterfaceItemImpl(tcls.getName(), qualifiedName(tcls), relativeName(tcls)));
            else if (tcls.isEnum())
              envitems.put(tcls, new EnumItemImpl(tcls.getName(), qualifiedName(tcls), relativeName(tcls)));
            else
              envitems.put(tcls, new ClassItemImpl(tcls.getName(), qualifiedName(tcls), relativeName(tcls)));
          }
          env_inner = (Type)envitems.get(tcls);
        }
      } else {
        assert inner instanceof PsiPrimitiveType;
        
        if (inner == PsiType.BOOLEAN)
          env_inner = BooleanType$.MODULE$;
        else if (inner == PsiType.INT)
          env_inner = IntType$.MODULE$;
        else if (inner == PsiType.CHAR)
          env_inner = CharType$.MODULE$;
        else if (inner == PsiType.FLOAT)
          env_inner = FloatType$.MODULE$;
        else if (inner == PsiType.DOUBLE)
          env_inner = DoubleType$.MODULE$;
        else if (inner == PsiType.LONG)
          env_inner = LongType$.MODULE$;
        else if (inner == PsiType.SHORT)
          env_inner = ShortType$.MODULE$;
        else if (inner == PsiType.VOID)
          env_inner = VoidType$.MODULE$;
        else {
          logger.error("Unknown primitive type: " + inner.getCanonicalText());
          return new ErrorType();
        }
      }
      assert env_inner != null;

      if (t instanceof PsiArrayType) {
        int dims = t.getArrayDimensions();
        if (env_inner instanceof InterfaceType) {
          types.put(t, makeArray((InterfaceType)env_inner, dims));
        } else if (env_inner instanceof EnumType) {
          types.put(t, makeArray((EnumType)env_inner, dims));
        } else if (env_inner instanceof ClassType) {
          types.put(t, makeArray((ClassType)env_inner, dims));
        } else if (env_inner instanceof ErrorType) {
          return new ErrorType();
        } else {
          // this is a primitive type, but which one?
          if (env_inner.name().equals("boolean"))
            types.put(t, makeArray((BooleanType$)env_inner, dims));
          else if (env_inner.name().equals("int"))
            types.put(t, makeArray((IntType$)env_inner, dims));
          else if (env_inner.name().equals("char"))
            types.put(t, makeArray((CharType$)env_inner, dims));
          else if (env_inner.name().equals("float"))
            types.put(t, makeArray((FloatType$)env_inner, dims));
          else if (env_inner.name().equals("double"))
            types.put(t, makeArray((DoubleType$)env_inner, dims));
          else if (env_inner.name().equals("long"))
            types.put(t, makeArray((LongType$)env_inner, dims));
          else if (env_inner.name().equals("short"))
            types.put(t, makeArray((ShortType$)env_inner, dims));
          else if (env_inner.name().equals("void"))
            types.put(t, makeArray((VoidType$)env_inner, dims));
          else {
            logger.error("Unknown primitive type: " + env_inner.qualifiedName());
            return new ErrorType();
          }
        }
        assert types.get(t) instanceof Type;
        return (Type) types.get(t);
      } else {
        // inner is our original type
        return env_inner;
      }
    } else {
      assert types.get(t) instanceof Type;
      return (Type) types.get(t);
    }
  }

  /**
   * Make the IntelliJ-independent class that is used by the tarksi engine to look up possible names
   */
  public JavaEnvironment getJavaEnvironment() {
    Map<PsiElement, NamedItem> envitems = new HashMap<PsiElement, NamedItem>();
    Map<PsiType, NamedItem> types = new HashMap<PsiType, NamedItem>();

    // first, register packages (we may need those as containing elements in classes)
    for (PsiPackage pkg : packages) {
      envitems.put(pkg, new PackageItemImpl(pkg.getName(), qualifiedName(pkg), relativeName(pkg)));

      // TODO: register everything that is below this package (some of which may already be in the env map)
    }

    // then, register classes (we need those as containing elements in the other things)
    // classes may be contained in classes, so partial-order the list first
    for (PsiClass cls : classes) {
      // TODO: get type parameters etc
      envitems.put(cls, new ClassItemImpl(cls.getName(), qualifiedName(cls), relativeName(cls)));

      // TODO: register everything that is below this class (some of which may already be in the env map)
    }

    // register methods (also register types used in this method)
    for (PsiMethod method : methods) {
      // get argument types
      List<Type> params = new SmartList<Type>();
      for (PsiParameter p : method.getParameterList().getParameters()) {
        params.add(addTypeToEnvMap(envitems, types, p.getType()));
      }

      if (method.isConstructor()) {
        // TODO: varargs
        // TODO: get type parameters
        // TODO: what to do with parameters depending on type parameters and bounded types and such?
        // get class
        PsiClass cls = method.getContainingClass();
        assert cls != null;
        if (!envitems.containsKey(cls)) {
          envitems.put(cls, new ClassItemImpl(cls.getName(), qualifiedName(cls), relativeName(cls)));
        }
        assert envitems.get(cls) instanceof ClassType;
        envitems.put(method, new ConstructorItem((ClassType)envitems.get(cls), scala.collection.JavaConversions.asScalaBuffer(params).toList()));
      } else {
        // TODO: varargs
        // TODO: get type parameters
        // TODO: what to do with parameters depending on type parameters and bounded types and such?
        Type rtype = addTypeToEnvMap(envitems, types, method.getReturnType());
        envitems.put(method, new MethodItemImpl(method.getName(), qualifiedName(method), relativeName(method), rtype, scala.collection.JavaConversions.asScalaBuffer(params).toList()));
      }
    }

    // then, register objects which have types (enum constants, variables, parameters, fields), and their types
    for (PsiVariable var : variables) {
      if (var instanceof PsiParameter) {
        envitems.put(var, new ParameterItemImpl(var.getName(), addTypeToEnvMap(envitems, types, var.getType())));
      } else if (var instanceof PsiEnumConstant)
        envitems.put(var, new EnumConstantItem(var.getName(), (EnumType)addTypeToEnvMap(envitems, types, var.getType())));
      else if (var instanceof PsiLocalVariable)
        envitems.put(var, new LocalVariableItemImpl(var.getName(), addTypeToEnvMap(envitems, types, var.getType())));
      else if (var instanceof PsiField) {
        assert envitems.containsKey(((PsiField) var).getContainingClass());
        envitems.put(var, new FieldItemImpl(var.getName(), addTypeToEnvMap(envitems, types, var.getType()), (ClassType) envitems.get(((PsiField) var).getContainingClass()), qualifiedName(var), relativeName(var)));
      }
    }

    return Tarski.environment(types.values(),envitems.values());
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
    else if (elem instanceof PsiMethod)
      return ((PsiMethod) elem).getContainingClass().getQualifiedName() + '.' + ((PsiMethod) elem).getName();
    else if (elem instanceof PsiEnumConstant)
      return ((PsiEnumConstant) elem).getContainingClass().getQualifiedName() + '.' + ((PsiEnumConstant) elem).getName();
    else if (elem instanceof PsiField)
      return ((PsiField) elem).getContainingClass().getQualifiedName() + '.' + ((PsiField) elem).getName();

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
    if (event == JavaScopeProcessorEvent.START_STATIC)
      inStaticScope = true;
    else if (event == JavaScopeProcessorEvent.SET_CURRENT_FILE_CONTEXT)
      currentFileContext = (PsiElement)associated;
  }
}
