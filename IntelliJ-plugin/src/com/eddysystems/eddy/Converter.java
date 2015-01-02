package com.eddysystems.eddy;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.roots.FileIndexFacade;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiClassReferenceType;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import scala.Option;
import scala.Some;
import scala.collection.JavaConversions;
import scala.collection.immutable.List$;
import scala.collection.immutable.Map$;
import scala.runtime.AbstractFunction0;
import tarski.Items.*;
import tarski.Types.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Converter {
  public final Place place;
  public final EnvironmentProcessor.JavaEnvironment jenv;
  public final Map<PsiElement, Item> locals; // We will add to this
  public final Map<PsiClass, ConstructorItem> localImplicitConstructors; // Implicit constructors, indexed by their PsiClass

  private final @NotNull Logger logger = Logger.getInstance(getClass());

  Converter(Place place,
            EnvironmentProcessor.JavaEnvironment jenv,
            Map<PsiElement, Item> locals, Map<PsiClass,ConstructorItem> localImplicitConstructors) {
    this.place = place;
    this.jenv = jenv;
    this.locals = locals;
    this.localImplicitConstructors = localImplicitConstructors;
  }

  @Nullable Item lookup(PsiElement e) {
    Item i = jenv.lookup(e);
    return i != null ? i : locals.get(e);
  }

  @Nullable ConstructorItem lookupImplicitConstructor(PsiClass c) {
    ConstructorItem i = jenv.lookupImplicitConstructor(c);
    return i != null ? i : localImplicitConstructors.get(c);
  }

  TypeArg convertTypeArg(PsiType t, Parent parent) {
    if (t instanceof PsiWildcardType) {
      PsiType bound = ((PsiWildcardType) t).getBound();
      if (!((PsiWildcardType)t).isSuper())
        return new WildSub(bound != null ? (RefType)convertType(bound, parent) : ObjectType$.MODULE$);
      else
        return new WildSuper(bound != null ? (RefType)convertType(bound, parent) : NullType$.MODULE$);
    }

    // can't have primitive types as type arguments
    assert !(t instanceof PsiPrimitiveType);
    return (RefType)convertType(t, parent);
  }

  // If parent is given, use it for generics resolution.  If it is null, use containing().inside() instead.
  // TODO: The version without parent should probably be deleted.
  Type convertType(PsiType t) {
    return convertType(t,null);
  }
  Type convertType(PsiType t, Parent parent) {
    // TODO: Handle modifiers
    if (t instanceof PsiArrayType)
      return new ArrayType(convertType(((PsiArrayType) t).getComponentType(), parent));

    // can't have wildcards in as regular types
    assert !(t instanceof PsiWildcardType);

    // Classes are not types in IntelliJ's version of the world, so we have to look up this class in envitems
    if (t instanceof PsiClassType) {
      PsiClass tcls = ((PsiClassType)t).resolve();
      if (tcls == null) {
        // we don't know how to deal with unresolved things that are not references.
        assert t instanceof PsiClassReferenceType;
        // ClassType cannot be resolved to a Class (probably because its class file is missing or the code is incomplete)
        return new UnresolvedClassItem(this, (PsiClassReferenceType)t, parent).generic();
      } else if (tcls instanceof PsiTypeParameter) {
        return addTypeParam((PsiTypeParameter) tcls);
      } else {
        List<TypeArg> jparams = new SmartList<TypeArg>();
        for (PsiType tp : ((PsiClassType)t).getParameters())
          jparams.add(convertTypeArg(tp,parent));
        scala.collection.immutable.List<TypeArg> params = scala.collection.JavaConversions.asScalaBuffer(jparams).toList();

        //logger.debug("converting class " + t + " with type parameters " + params.mkString("[",",","]"));

        ClassItem item = (ClassItem)addClass(tcls,false,false);

        //logger.debug("  item: " + item + ": params " + item.tparams().mkString("[",",","]"));

        assert params.size() == ((PsiClassType)t).getParameterCount();

        if (item.arity() > 0 && params.isEmpty()) {
          // This happens. For instance in java.lang.SecurityManager.getClassContext()
          return item.raw();
        } else if (parent == null) {
          ParentItem container = addContainer(place.containing(tcls));
          return item.generic(params,container.inside());
        } else {
          return item.generic(params,parent);
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

  ParentItem addContainer(PsiElement elem) {
    {
      Item i = lookup(elem);
      if (i != null)
        return (ParentItem)i;
    }
    if (elem == null)
      return tarski.Tarski.localPkg();

    // local classes
    if (elem instanceof PsiMethod)
      return addMethod((PsiMethod) elem);
    if (elem instanceof PsiClass)
      return (ParentItem)addClass((PsiClass) elem, false, false);
    else if (elem instanceof PsiPackage) {
      final PsiPackage pkg = (PsiPackage)elem;
      if (pkg.getName() == null)
        return tarski.Tarski.localPkg();
      final PackageItem item = new PackageItem(pkg.getName(),pkg.getQualifiedName());
      locals.put(pkg,item);
      return item;
    }
    throw new RuntimeException("weird container "+elem);
  }

  protected interface PsiEquivalent {
    PsiElement psi();
  }

  protected interface ReferencingItem {
    // eliminate the reference to it (by flushing caches, to be replaced)
    // returns whether found the item anywhere where it may affect inheritance (namely, in _superItems)
    boolean flushReference(Item it);

    // check for UnresolvedClassItem and replace reference to it if it is now resolved
    // returns whether things were replaced that may affects inheritance (namely, in _superItems)
    boolean fillUnresolvedReferences();
  }

  // utility functions to be used by fillUnresolvedReferences (would be in ReferencingItem if Java allowed it)

  // for hi, base
  static RefType checkReplaceUnresolved(RefType ref) {
    if (ref == null)
      return ref;
    if (ref.item() instanceof UnresolvedClassItem && ((UnresolvedClassItem) ref.item()).resolve() != null) {
      return ((UnresolvedClassItem) ref.item()).generic();
    } else {
      return ref;
    }
  }

  // for superItems
  static boolean checkUnresolvedItems(scala.collection.immutable.List<RefTypeItem> refs) {
    if (refs == null)
      return false;
    for (int i = 0; i < refs.length(); ++i) {
      RefTypeItem ref = refs.apply(i);
      if (ref instanceof UnresolvedClassItem && ((UnresolvedClassItem) ref).resolve() != null) {
        return true;
      }
    }
    return false;
  }
  // only call this if checkUnresolvedItems returned true!
  static scala.collection.immutable.List<RefTypeItem> checkReplaceUnresolvedItems(scala.collection.immutable.List<RefTypeItem> refs) {
    ArrayList<RefTypeItem> jrefs = new ArrayList<RefTypeItem>(refs.length());
    for (int i = 0; i < refs.length(); ++i) {
      RefTypeItem ref = refs.apply(i);
      if (ref instanceof UnresolvedClassItem && ((UnresolvedClassItem) ref).resolve() != null) {
        jrefs.add(((UnresolvedClassItem) ref).resolve());
      } else {
        jrefs.add(ref);
      }
    }
    return JavaConversions.asScalaBuffer(jrefs).toList();
  }

  // for supers, params (should be: X extends Type super ClassType)
  static <X extends Type> scala.collection.immutable.List<X> checkReplaceUnresolvedTypes(scala.collection.immutable.List<X> refs) {
    if (refs == null)
      return refs;
    boolean replace = false;
    for (int i = 0; i < refs.length(); ++i) {
      Type t = refs.apply(i);
      if (!(t instanceof RefType))
        continue;

      RefTypeItem ref = ((RefType) t).item();
      if (ref instanceof UnresolvedClassItem && ((UnresolvedClassItem) ref).resolve() != null) {
        replace = true;
      }
    }
    if (replace) {
      ArrayList<X> jrefs = new ArrayList<X>(refs.length());
      for (int i = 0; i < refs.length(); ++i) {
        X rt = refs.apply(i);
        if (rt instanceof RefType) {
          RefTypeItem ref = ((RefType)rt).item();
          if (ref instanceof UnresolvedClassItem && ((UnresolvedClassItem) ref).resolve() != null) {
            jrefs.add((X)((UnresolvedClassItem) ref).generic());
          } else {
            jrefs.add(rt);
          }
        } else {
          jrefs.add(rt);
        }
      }
      return JavaConversions.asScalaBuffer(jrefs).toList();
    } else {
      return refs;
    }
  }

  static protected class UnresolvedClassItem extends ClassItem implements PsiEquivalent, CachedNameItem, SettableFinalItem {
    @NotNull final Converter env;
    @NotNull final PsiClassReferenceType cls;

    @NotNull final Parent _parent;
    boolean _isFinal = false;
    scala.collection.immutable.List<TypeArg> _targs;
    scala.collection.immutable.List<TypeVar> _tparams = null;
    ClassItem _resolved = null;

    static final ConstructorItem[] noConstructors = new ConstructorItem[0];
    static final scala.collection.immutable.List<RefType> _supers = List$.MODULE$.fill(1, new AbstractFunction0<RefType>() {
      @Override
      public RefType apply() {
        return ObjectType$.MODULE$;
      }
    });
    static final scala.collection.immutable.List<RefTypeItem> _superItems = List$.MODULE$.fill(1, new AbstractFunction0<RefTypeItem>() {
      @Override
      public RefTypeItem apply() {
        return ObjectItem$.MODULE$;
      }
    });

    UnresolvedClassItem(@NotNull final Converter env, @NotNull final PsiClassReferenceType cls, @Nullable final Parent parent) {
      this.env = env;
      this.cls = cls;
      _parent = parent == null ? (PackageItem)env.addContainer(env.place.getElementPackage(cls.getReference())) : parent;

      if (cls instanceof PsiModifierListOwner)
        _isFinal = ((PsiModifierListOwner)cls).hasModifierProperty(PsiModifier.FINAL);

      List<TypeArg> jargs = new SmartList<TypeArg>();
      for (PsiType arg : cls.getParameters())
        jargs.add(env.convertTypeArg(arg,_parent));
      _targs = scala.collection.JavaConversions.asScalaBuffer(jargs).toList();
    }

    public String name() { return cls.getClassName(); }
    public boolean isClass() { return true; }
    public boolean isEnum() { return false; }
    public boolean isFinal() { return _isFinal; }
    public ParentItem parent() { return _parent.item(); }
    public ClassType base() { return ObjectType$.MODULE$; }
    public scala.collection.immutable.List<RefType> supers() { return _supers; }
    public scala.collection.immutable.List<RefTypeItem> superItems() { return _superItems; }

    public scala.collection.immutable.List<TypeVar> tparams() {
      if (_tparams == null) {
        List<TypeVar> jargs = new SmartList<TypeVar>();
        for (int i = 0; i < _targs.length(); ++i)
          jargs.add(new SimpleTypeVar("T" + i));
        _tparams = scala.collection.JavaConversions.asScalaBuffer(jargs).toList();
      }
      return _tparams;
    }

    public ClassType generic() {
      if (_resolved == null) {
        return generic(_targs, _parent);
      } else {
        return _resolved.generic(_targs, _parent);
      }
    }

    public ClassItem resolve() {
      if (_resolved == null) {
        // try to resolve reference, will remain null if it fails
        _resolved = (ClassItem)env.lookup(cls.resolve());
      }
      return _resolved;
    }

    public boolean declaresField(String kid) {
      return false;
    }

    public ConstructorItem[] constructors() {
      return noConstructors;
    }

    public void refreshName() { /* not caching */ }

    public PsiElement psi() {
      return cls.getReference();
    }

    public void setFinal(boolean f) {
      _isFinal = f;
    }

    public boolean equals(Object x) { return this == x; }
    public boolean canEqual(Object x) { return this == x; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }


  static protected class LazyTypeVar extends TypeVar implements PsiEquivalent, ReferencingItem, CachedNameItem, CachedSupersItem {
    private final Converter env;
    private final PsiTypeParameter p;

    // Lazy fields
    private RefType _hi;
    private scala.collection.immutable.List<RefTypeItem> _superItems;

    LazyTypeVar(Converter env, PsiTypeParameter p) {
      this.env = env;
      this.p = p;
    }

    public String name() {
      return p.getName();
    }

    public RefType lo() {
      return NullType$.MODULE$;
    }

    public RefType hi() {
      if (_hi == null) {
        List<ClassType> supers = new SmartList<ClassType>();
        for (PsiClassType e : p.getExtendsList().getReferencedTypes())
          supers.add((ClassType)env.convertType(e));
        _hi = tarski.Types.glb(JavaConversions.asScalaBuffer(supers).toList());
      }
      return _hi;
    }

    public scala.collection.immutable.List<RefTypeItem> superItems() {
      if (_superItems == null) {
        List<RefTypeItem> supers = new SmartList<RefTypeItem>();
        for (PsiClass c : p.getSupers())
          supers.add(env.addClass(c,false,false));
        _superItems = JavaConversions.asScalaBuffer(supers).toList();
      }
      return _superItems;
    }

    public void invalidateSupers() {
      _hi = null;
      _superItems = null;
    }

    public void refreshName() {} // we don't actually cache the name

    public boolean flushReference(Item it) {
      if (_hi != null && _hi.item() == it) {
        _hi = null;
      }
      if (it instanceof RefTypeItem && _superItems != null && _superItems.contains(it)) {
        _superItems = null;
        return true;
      }
      return false;
    }

    public boolean fillUnresolvedReferences() {
      _hi = checkReplaceUnresolved(_hi);
      if (checkUnresolvedItems(_superItems)) {
        _superItems = checkReplaceUnresolvedItems(_superItems);
        return true;
      } else
        return false;
    }

    public PsiElement psi() {
      return p;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean equals(Object x) { return this == x; }
    public boolean canEqual(Object x) { return this == x; }
    public Some safe() { return new Some<RefType>(this); }
    public scala.collection.immutable.List<tarski.Denotations.Stmt> discards() { return _discards; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }

    // For use above
    static scala.collection.immutable.List<tarski.Denotations.Stmt> _discards =
      JavaConversions.asScalaBuffer(new ArrayList<tarski.Denotations.Stmt>()).toList();

  }

  private TypeVar addTypeParam(PsiTypeParameter p) {
    {
      Item i = lookup(p);
      if (i != null)
        return (TypeVar)i;
    }
    // Use a maker to break recursion
    TypeVar ti = new LazyTypeVar(this,p);
    locals.put(p, ti);
    return ti;
  }

  static protected class LazyClass extends ClassItem implements PsiEquivalent, ReferencingItem, CachedNameItem, CachedConstructorsItem, CachedTypeParametersItem, CachedBaseItem, CachedSupersItem, SettableFinalItem {
    private final Converter env;
    private final PsiClass cls;
    private final ParentItem _parent;
    private boolean _isFinal;
    private String _name;

    // Lazy fields
    private scala.collection.immutable.List<TypeVar> _tparams;
    private ClassType _base;
    private scala.collection.immutable.List<RefType> _supers;
    private scala.collection.immutable.List<RefTypeItem> _superItems;
    private ConstructorItem[] _constructors;

    LazyClass(Converter env, PsiClass cls, ParentItem parent) {
      this.env = env;
      this.cls = cls;
      this._name = cls.getName();
      this._parent = parent;
      this._isFinal = cls.hasModifierProperty(PsiModifier.FINAL);
    }

    public String name() {
      return _name;
    }

    public scala.collection.immutable.List<TypeVar> tparams() {
      if (_tparams == null)
        _tparams = env.tparams(cls);
      return _tparams;
    }

    public boolean isFinal() {
      return _isFinal;
    }

    public boolean isClass() {
      return !cls.isInterface();
    }

    public boolean isEnum() {
      return cls.isEnum();
    }

    public ParentItem parent() {
      return _parent;
    }

    public ClassType base() {
      if (_base == null)
        supers();
      return _base;
    }

    public scala.collection.immutable.List<RefType> supers() {
      if (_supers == null) {
        PsiClass base = cls.getSuperClass();
        ArrayList<RefType> supers = new ArrayList<RefType>();
        for (PsiClassType stype : cls.getSuperTypes()) {
          ClassType sc = (ClassType)env.convertType(stype);
          PsiClass stypeClass = stype.resolve();
          if (base == stypeClass)
            _base = sc;
          supers.add(sc);
        }
        if (_base == null) {
          _base = ((ClassItem)env.addClass(base,false,false)).inside();
        }
        _supers = JavaConversions.asScalaBuffer(supers).toList();
      }
      return _supers;
    }

    public scala.collection.immutable.List<RefTypeItem> superItems() {
      if (_superItems == null) {
        ArrayList<RefTypeItem> supers = new ArrayList<RefTypeItem>();
        for (PsiClass s : cls.getSupers())
          supers.add(env.addClass(s, false, false));
        _superItems = JavaConversions.asScalaBuffer(supers).toList();
      }
      return _superItems;
    }

    public boolean declaresField(String kid) {
      return cls.findFieldByName(kid, false) != null;
    }

    public ConstructorItem[] constructors() {
      if (_constructors == null) {
        final ArrayList<ConstructorItem> cons = new ArrayList<ConstructorItem>();
        for (PsiMethod m : cls.getConstructors())
          cons.add((ConstructorItem)env.addMethod(m));

        // add implicit constructor if no others declared
        if (!cons.isEmpty()) {
          cons.add(env.addImplicitConstructor(cls, this));
        }

        _constructors = cons.toArray(new ConstructorItem[cons.size()]);
      }
      return _constructors;
    }

    public void invalidateConstructors() {
      _constructors = null;
    }

    public void invalidateTypeParameters() {
      _tparams = null;
      // these may depend on the tparams list and should be updated
      _base = null;
      _supers = null;
      _superItems = null;
    }

    public void invalidateBase() {
      _base = null;
      _supers = null;
      _superItems = null;
    }

    public void invalidateSupers() {
      _supers = null;
      _superItems = null;
    }

    public void setFinal(boolean b) {
      _isFinal = b;
    }

    public void refreshName() {
      _name = cls.getName();
    }

    public boolean flushReference(Item it) {
      if (_parent != null && _parent == it)
        throw new RuntimeException("Can't eliminate reference to parent " + _parent + ", this (" + this + ") should have been deleted before parent.");

      if (_base != null && _base.item() == it)
        _base = null;

      if (it instanceof TypeVar && _tparams != null && _tparams.contains(it))
        _tparams = null;

      if (it instanceof RefTypeItem && _superItems != null) {
        for (int i = 0; i < _superItems.size(); ++i) {
          if (_superItems.apply(i) == it) {
            _supers = null;
            _superItems = null;
            return true;
          }
        }
      }

      return false;
    }

    public boolean fillUnresolvedReferences() {
      _base = (ClassType)checkReplaceUnresolved(_base);
      _supers = checkReplaceUnresolvedTypes(_supers);
      if (checkUnresolvedItems(_superItems)) {
        _superItems = checkReplaceUnresolvedItems(_superItems);
        return true;
      } else
        return false;
    }

    public PsiElement psi() {
      return cls;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean equals(Object x) { return this == x; }
    public boolean canEqual(Object x) { return this == x; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  // used to dynamically add items from Psi tree change events
  Item addItem(PsiElement elem) {
    if (elem instanceof PsiClass) {
      return addClass((PsiClass)elem, true, true);
    } else if (elem instanceof PsiMethod) {
      return addMethod((PsiMethod)elem);
    } else if (elem instanceof PsiField) {
      return addField((PsiField)elem);
    } else if (elem instanceof PsiPackage) {
      return addContainer(elem);
    } else {
      throw new RuntimeException("adding unknown item type: " + elem);
    }
  }

  RefTypeItem addClass(PsiClass cls, boolean recurse, boolean noProtected) {
    {
      Item i = lookup(cls);
      if (i != null) {
        if (recurse) // if we are forced to recurse, we may still have to force fields that were previously unseen
          addClassMembers(cls, (ClassItem)i, noProtected);
        return (RefTypeItem)i;
      }
    }

    if (FileIndexFacade.getInstance(place.project).isInSourceContent(cls.getContainingFile().getVirtualFile())) {
      System.out.println("adding local class " + cls);
    }

    if (cls instanceof PsiTypeParameter)
      return addTypeParam((PsiTypeParameter)cls);

    // Make and add the class
    final ParentItem parent = addContainer(place.containing(cls));
    ClassItem item = new LazyClass(this,cls,parent);
    locals.put(cls,item);

    if (recurse)
      addClassMembers(cls,item,noProtected);
    return item;
  }

  ConstructorItem addImplicitConstructor(PsiClass cls, ClassItem item) {
    ConstructorItem ci = lookupImplicitConstructor(cls);
    if (ci == null) {
      ci = new DefaultConstructorItem(item);
      localImplicitConstructors.put(cls, ci);
    }
    return ci;
  }

  void addClassMembers(PsiClass cls, ClassItem item, boolean noProtected) {
    final Set<String> set = item instanceof BaseItem ? ((BaseItem)item).fieldNames() : null;
    for (PsiField f : cls.getFields())
      if (!place.isInaccessible(f,noProtected)) {
        if (set != null)
          set.add(f.getName());
        addField(f);
      }
    for (PsiMethod m : cls.getMethods())
      if (!place.isInaccessible(m,noProtected) && !m.isConstructor()) // constructors are added below
        addMethod(m);

    boolean foundConstructor = false;
    for (PsiMethod m : cls.getConstructors()) {
      foundConstructor = true;
      if (!place.isInaccessible(m,noProtected))
        addMethod(m);
    }
    if (!foundConstructor)
      addImplicitConstructor(cls, item);

    for (PsiClass c : cls.getInnerClasses())
      if (!place.isInaccessible(c,noProtected))
        addClass(c,true,noProtected);
  }

  private scala.collection.immutable.List<TypeVar> tparams(PsiTypeParameterListOwner owner) {
    List<TypeVar> jtparams = new ArrayList<TypeVar>();
    for (PsiTypeParameter tp : owner.getTypeParameters())
      jtparams.add(addTypeParam(tp));
    return scala.collection.JavaConversions.asScalaBuffer(jtparams).toList();
  }

  private scala.collection.immutable.List<Type> params(PsiMethod method) {
    List<Type> jparams = new SmartList<Type>();
    for (PsiParameter p : method.getParameterList().getParameters())
      jparams.add(convertType(p.getType()));
    return scala.collection.JavaConversions.asScalaBuffer(jparams).toList();
  }

  protected static class LazyConstructor extends ConstructorItem implements PsiEquivalent, ReferencingItem, CachedTypeParametersItem, CachedParametersItem {
    private final Converter env;
    private final PsiMethod method;

    // Lazy fields (null initially)
    private ClassItem _parent;
    private scala.collection.immutable.List<TypeVar> _tparams;
    private scala.collection.immutable.List<Type> _params;

    LazyConstructor(Converter env, PsiMethod method) {
      this.env = env;
      this.method = method;
    }

    // Core interface
    public ClassItem parent() {
      if (_parent == null)
        _parent = (ClassItem)env.addClass(method.getContainingClass(),false,false);
      return _parent;
    }
    public scala.collection.immutable.List<TypeVar> tparams() {
      if (_tparams == null)
        _tparams = env.tparams(method);
      return _tparams;
    }
    public scala.collection.immutable.List<Type> params() {
      if (_params == null)
        _params = env.params(method);
      return _params;
    }

    public void invalidateTypeParameters() {
      _tparams = null;
      // this may affect our parameter list, so redo that as well
      _params = null;
    }

    public void invalidateParameters() {
      _params = null;
    }

    public boolean flushReference(Item it) {
      if (_parent != null && _parent == it)
        throw new RuntimeException("Can't eliminate reference to parent " + _parent + ", this (" + this + ") should have been deleted before parent.");

      if (it instanceof TypeVar && _tparams != null && _tparams.contains(it))
        _tparams = null;

      if (it instanceof TypeItem && _params != null) {
        for (int i = 0; i < _params.size(); ++i) {
          if (_params.apply(i).item() == it) {
            _params = null;
            break;
          }
        }
      }
      return false;
    }

    public boolean fillUnresolvedReferences() {
      _params = checkReplaceUnresolvedTypes(_params);
      return false;
    }

    public PsiElement psi() {
      return method;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean equals(Object x) { return this == x; }
    public boolean canEqual(Object x) { return this == x; }
    public Parent simple() { throw new RuntimeException("For ConstructorItem, only inside is valid, not simple"); }
    public Option<Parent> safe() { return new Some<Parent>(this); }
    public scala.collection.immutable.Map<TypeVar,Option<RefType>> env() { return Map$.MODULE$.empty(); }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  protected static class LazyMethod extends MethodItem implements PsiEquivalent, ReferencingItem, CachedNameItem, SettableStaticItem, CachedTypeParametersItem, CachedParametersItem, CachedReturnTypeItem {
    final Converter env;
    final PsiMethod method;
    private String _name;
    private boolean _isStatic;

    // Lazy fields (null initially)
    private ClassItem _parent;
    private scala.collection.immutable.List<TypeVar> _tparams;
    private scala.collection.immutable.List<Type> _params;
    private Type _retVal;

    LazyMethod(Converter env, PsiMethod method) {
      this.env = env;
      this.method = method;
      this._name = method.getName();
      this._isStatic = method.hasModifierProperty(PsiModifier.STATIC);
    }

    // Core interface
    public String name() {
      return _name;
    }
    public boolean isStatic() {
      return _isStatic;
    }
    public ClassItem parent() {
      if (_parent == null)
        _parent = (ClassItem)env.addClass(method.getContainingClass(),false,false);
      return _parent;
    }
    public scala.collection.immutable.List<TypeVar> tparams() {
      if (_tparams == null)
        _tparams = env.tparams(method);
      return _tparams;
    }
    public scala.collection.immutable.List<Type> params() {
      if (_params == null)
        _params = env.params(method);
      return _params;
    }
    public Type retVal() {
      if (_retVal == null)
        if (method.getReturnType() == null) // this happens in invalid code, while fiddling with a declaration
          _retVal = VoidType$.MODULE$;
        else
          _retVal = env.convertType(method.getReturnType());
      return _retVal;
    }

    public void setStatic(boolean b) {
      _isStatic = b;
    }

    public void invalidateTypeParameters() {
      _tparams = null;
      // this may affect our parameter list, so redo that as well
      _retVal = null;
      _params = null;
    }

    public void invalidateParameters() {
      _params = null;
    }

    public void invalidateReturnType() {
      _retVal = null;
    }

    public void refreshName() {
      _name = method.getName();
    }

    public boolean flushReference(Item it) {
      if (_parent != null && _parent == it)
        throw new RuntimeException("Can't eliminate reference to parent " + _parent + ", this (" + this + ") should have been deleted before parent.");

      if (it instanceof TypeVar && _tparams != null &&  _tparams.contains(it))
        _tparams = null;

      if (it instanceof TypeItem && _params != null) {
        for (int i = 0; i < _params.size(); ++i) {
          if (_params.apply(i).item() == it) {
            _params = null;
            break;
          }
        }
      }

      if (_retVal != null && _retVal.item() == it)
        _retVal = null;

      return false;
    }

    public boolean fillUnresolvedReferences() {
      if (_retVal != null && _retVal instanceof RefType)
        _retVal = checkReplaceUnresolved((RefType)_retVal);
      _params = checkReplaceUnresolvedTypes(_params);
      return false;
    }

    public PsiElement psi() {
      return method;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean equals(Object x) { return this == x; }
    public boolean canEqual(Object x) { return this == x; }
    public Parent simple() { throw new RuntimeException("For ConstructorItem, only inside is valid, not simple"); }
    public Option<Parent> safe() { return new Some<Parent>(this); }
    public scala.collection.immutable.Map<TypeVar,Option<RefType>> env() { return Map$.MODULE$.empty(); }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  CallableItem addMethod(PsiMethod method) {
    {
      Item i = lookup(method);
      if (i != null)
        return (CallableItem)i;
    }

    CallableItem item = method.isConstructor() ? new LazyConstructor(this,method)
                                               : new LazyMethod(this,method);
    locals.put(method,item);
    return item;
  }

  protected static class LazyField extends FieldItem implements PsiEquivalent, ReferencingItem, CachedNameItem, SettableFinalItem, SettableStaticItem {
    private final Converter env;
    private final PsiField f;
    private String _name;
    private boolean _isFinal;
    private boolean _isStatic;

    // Lazy fields
    private Type _inside;

    LazyField(Converter env, PsiField f, boolean isFinal, boolean isStatic) {
      this.env = env;
      this.f = f;
      this._name = f.getName();
      this._isFinal = isFinal;
      this._isStatic = isStatic;
    }

    public String name() {
      return _name;
    }
    public boolean isFinal() {
      return _isFinal;
    }
    public boolean isStatic() { return _isStatic; }
    public Type inside() {
      if (_inside == null) {
        try {
          _inside = env.convertType(f.getType());
        } catch (Exception e) {
          throw new RuntimeException("LazyField::inside failed: field "+qualifiedName().get()+": "+e.getMessage());
        }
      }
      return _inside;
    }
    public ClassItem parent() {
      return (ClassItem)env.addClass(f.getContainingClass(),false,false);
    }

    public void setFinal(boolean b) { _isFinal = b; }
    public void setStatic(boolean b) { _isStatic = b; }

    public void refreshName() {
      _name = f.getName();
    }

    public boolean flushReference(Item it) {
      if (_inside != null && _inside.item() == it)
        _inside = null;
      return false;
    }

    public boolean fillUnresolvedReferences() {
      if (_inside != null && _inside instanceof RefType)
        _inside = checkReplaceUnresolved((RefType)_inside);
      return false;
    }


    public PsiElement psi() {
      return f;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean equals(Object x) { return this == x; }
    public boolean canEqual(Object x) { return this == x; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  Value addField(PsiField f) {
    {
      Item i = lookup(f);
      if (i != null)
        return (Value)i;
    }

    Value v;
    if (f instanceof PsiEnumConstant) {
      ClassItem c = (ClassItem)addClass(f.getContainingClass(),false,false);
      v = new EnumConstantItem(f.getName(),c);
    } else {
      boolean isFinal = f.hasModifierProperty(PsiModifier.FINAL);
      boolean isStatic = f.hasModifierProperty(PsiModifier.STATIC);
      v = new LazyField(this,f,isFinal,isStatic);
    }
    locals.put(f,v);
    return v;
  }
}
