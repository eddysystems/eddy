import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;

import tarski.Environment;

import java.util.HashMap;
import java.util.HashSet;
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
  private final List<PsiEnumConstant> enums = new SmartList<PsiEnumConstant>();
  private final List<PsiElement> elements = new SmartList<PsiElement>();

  // the expanded symbol dictionary
  private List<EnvironmentInfo> environment = null;

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

  /**
   * Make the IntelliJ-independent class that is used by the tarksi engine to look up possible names
   */
  public Environment.JavaEnvironment getJavaEnvironment() {
    Environment.JavaEnvironment environment = new Environment.JavaEnvironment();

    Map<PsiElement, Environment.NamedItem> envitems = new HashMap<PsiElement, Environment.NamedItem>();

    // register basic types
    // TODO

    // first, register packages (we may need those as containing elements in classes)
    for (PsiPackage pkg : packages) {
      envitems.put(pkg, new Environment.PackageItemImpl(pkg.getName(), qualifiedName(pkg), relativeName(pkg)));
    }

    // then, register classes (we need those as containing elements in the other things)
    // classes may be contained in classes, so partial-order the list first
    for (PsiClass cls : classes) {
      envitems.put(cls, new Environment.ClassItemImpl(cls.getName(), qualifiedName(cls), relativeName(cls)));
    }

    /*

    // then, register everything else (enums, methods, variables)
    for (PsiVariable var : variables) {
      if (var instanceof PsiParameter) {
        PsiType tvar = typeOf(var);
        if (!envitems.containsKey(tvar))
          envitems.put(tvar, new Environment.TypeItemImpl(tvar.getCanonicalText()))
        envitems.put(var, new Environment.ParameterItemImpl(var.getName(), (Environment.TypeItem) envitems.get(var.getTypeElement())));
      } else if (var instanceof PsiEnumConstant)
        envitems.put(var, new Environment.EnumConstantItemImpl(var.getName(), envitems.get(var.getTypeElement()), qualifiedName(var), relativeName(var)));
      else if (var instanceof PsiLocalVariable)
        envitems.put(var, new Environment.LocalVariableItemImpl(var.getName()));
      else if (var instanceof PsiField)
        envitems.put(var, new Environment.FieldItemImpl(var.getName(), qualifiedName(var), relativeName(var)));

    }

    // add items we found
    for (Environment.NamedItem thing : envitems.values()) {
      environment.addObject(thing);
    }
    */
    return environment;
  }

  /**
   * Expand the environment by enumerating all things that require qualification by something that's in scope. For
   * instance, If a class A is in scope, but place is not somewhere in A or a subclass of A, then a field x of A is
   * not in the environment. Calling expand fills a list of names, each associated with a list of ways to get to
   * qualify that name such that it is in scope. For instance, calling expand will result in an entry x: [A] to denote
   * that writing A.x would be legal.
   */
  class EnvironmentInfo {
    public PsiElement element;

    // a list of all paths to get to this element (there may be more than one)
    // each path starts with a symbol that's in scope. Concatenating the path with '.' and appending the element should
    // yield a name that correctly qualifies the element from the current place.
    // For shadowed elements, the path is just large enough to overcome the shadowing
    public List<PsiElement[]> paths = new SmartList<PsiElement[]>();

    EnvironmentInfo(PsiElement element) {
      this.element = element;
    }

    float score(String query) {
      // TODO: take into account how well the name fits the query and how deep the paths are + other heuristics
      return 0;
    }
  }

  public void expand() {
    // all elements in the environment are added with a zero length path
    environment = new SmartList<EnvironmentInfo>();

    HashSet<String> shadowed = new HashSet<String>();

    for (final PsiElement elem : getElements()) {
      // add all non-shadowed items into the environment
      String ident = name(elem);

      // for example, an anonymous class -- we can't do much with that, ignore it
      if (ident == null)
        continue;

      if (shadowed.contains(ident)) {
        // check whether we can unshadow this by using either this. or super. or some part of the fully qualified name
        // TODO
      }

      // TODO
    }

    List<PsiElement> queue = new SmartList<PsiElement>();
    queue.addAll(this.getElements());

    while (!queue.isEmpty()) {
      int idx = queue.size()-1;
      PsiElement element = queue.get(idx);
      queue.remove(idx);

      // find all sub-names of this name
      // TODO...
    }

    // fill environment
    // TODO
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

  public List<PsiClass> getClasses() {
    return classes;
  }

  public List<PsiVariable> getVariables() {
    return variables;
  }

  public List<PsiEnumConstant> getEnums() {
    return enums;
  }

  public List<PsiMethod> getMethods() {
    return methods;
  }

  public List<PsiPackage> getPackages() {
    return packages;
  }

  public List<PsiElement> getElements() {
    return elements;
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
      elements.add(element);
      classes.add((PsiClass)element);
    } else if (element instanceof PsiEnumConstant) {
      elements.add((PsiNameIdentifierOwner) element);
      enums.add((PsiEnumConstant)element);
    } else if (element instanceof PsiVariable) {
      elements.add((PsiNameIdentifierOwner) element);
      variables.add((PsiVariable)element);
    } else if (element instanceof PsiMethod) {
      elements.add((PsiNameIdentifierOwner) element);
      methods.add((PsiMethod)element);
    } else if (element instanceof PsiPackage) {
      elements.add((PsiNameIdentifierOwner) element);
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
