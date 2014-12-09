package com.eddysystems.eddy;

import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import scala.Option;
import scala.Some;
import scala.collection.JavaConversions;
import scala.collection.immutable.Map$;
import tarski.Items.*;
import tarski.Types.*;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.impl.source.PsiClassReferenceType;
import com.intellij.psi.*;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Converter {
  private final Place place;
  private final Map<PsiElement,Item> globals;
  public final Map<PsiElement, Item> locals; // Others will add to this

  private final @NotNull Logger logger = Logger.getInstance(getClass());

  Converter(Place place, Map<PsiElement, Item> globals, Map<PsiElement, Item> locals) {
    this.place = place;
    this.globals = globals;
    this.locals = locals;
  }

  @NotNull Item lookupFail(PsiElement e) {
    Item i = lookupNull(e);
    if (i != null)
      return i;

    if (e instanceof PsiClass) {
      PsiClass c = (PsiClass)e;
      throw new RuntimeException("Unregistered class "+e+", "+c.getQualifiedName());
    } else
      throw new RuntimeException("Unregistered item "+e);
  }

  @Nullable Item lookupNull(PsiElement e) {
    Item i = globals.get(e);
    return i != null ? i : locals.get(e);
  }

  Type convertType(PsiType t) {
    // TODO: Handle modifiers
    if (t instanceof PsiArrayType)
      return new ArrayType(convertType(((PsiArrayType) t).getComponentType()));

    if (t instanceof PsiWildcardType) {
      // TODO: need proper wildcard expressions
      PsiType bound = ((PsiWildcardType) t).getBound();
      return bound != null ? convertType(bound) : ObjectType$.MODULE$;
    }

    // Classes are not types in IntelliJ's version of the world, so we have to look up this class in envitems
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
        return addTypeParam((PsiTypeParameter) tcls);
      } else {
        List<TypeArg> jparams = new SmartList<TypeArg>();
        for (PsiType tp : ((PsiClassType)t).getParameters())
          jparams.add((TypeArg) convertType(tp));
        scala.collection.immutable.List<TypeArg> params = scala.collection.JavaConversions.asScalaBuffer(jparams).toList();

        //logger.debug("converting class " + t + " with type parameters " + params.mkString("[",",","]"));

        ClassItem item = (ClassItem)addClass(tcls,false,false);

        //logger.debug("  item: " + item + ": params " + item.tparams().mkString("[",",","]"));

        assert params.size() == ((PsiClassType)t).getParameterCount();

        if (item.arity() > 0 && params.isEmpty()) {
          // This happens. For instance in java.lang.SecurityManager.getClassContext()
          return item.raw();
        } else {
          Item container = addContainer(place.containing(tcls));
          assert container instanceof ParentItem;
          return item.generic(params,((ParentItem)container).inside());
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

  Item addContainer(PsiElement elem) {
    {
      Item i = lookupNull(elem);
      if (i != null)
        return i;
    }

    // local classes
    if (elem instanceof PsiMethod)
      return addMethod((PsiMethod) elem);
    if (elem instanceof PsiClass)
      return addClass((PsiClass) elem, false, false);
    else if (elem instanceof PsiPackage) {
      PsiPackage pkg = (PsiPackage)elem;
      String qual = pkg.getQualifiedName();
      Item base = tarski.Tarski.baseLookupJava(qual);
      PackageItem item = base != null ? (PackageItem)base : new PackageItem(pkg.getName(), qual);
      locals.put(pkg, item);
      return item;
    }
    throw new RuntimeException("weird container "+elem);
  }

  static private class LazyTypeVar extends TypeVar {
    private final Converter env;
    private final PsiTypeParameter p;
    private RefType _hi; // Starts out null (uninitialized)

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

    // Necessary only due to screwy Java/Scala interop
    public boolean canEqual(Object x) { return this == x; }
    public Some safe() { return new Some<RefType>(this); }
    public TypeVar raw() { throw new NotImplementedError("Should never happen"); }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  private TypeVar addTypeParam(PsiTypeParameter p) {
    {
      Item i = lookupNull(p);
      if (i != null)
        return (TypeVar)i;
    }
    // Use a maker to break recursion
    TypeVar ti = new LazyTypeVar(this,p);
    locals.put(p, ti);
    return ti;
  }

  private static class LazyClass extends ClassItem {
    private final Converter env;
    private final PsiClass cls;
    private final ParentItem _parent;
    private final boolean _isFinal;

    // Lazy fields
    private scala.collection.immutable.List<TypeVar> _tparams;
    private ClassType _base;
    private scala.collection.immutable.List<ClassType> _interfaces;
    private scala.collection.immutable.List<RefType> _supers;

    LazyClass(Converter env, PsiClass cls, ParentItem parent) {
      this.env = env;
      this.cls = cls;
      this._parent = parent;
      this._isFinal = cls.hasModifierProperty(PsiModifier.FINAL);
    }

    public String name() {
      return cls.getName();
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
      if (_base == null) {
        PsiClass base = cls.getSuperClass();
        // TODO: get actual type arguments for base and make generic class
        _base = (ClassType)env.addClass(base,false,false).raw();
      }
      return _base;
    }

    public scala.collection.immutable.List<ClassType> interfaces() {
      if (_interfaces == null) {
        ArrayList<ClassType> j_interfaces = new ArrayList<ClassType>();
        for (PsiClass i : cls.getInterfaces()) {
          // TODO: Handle generics
          j_interfaces.add(((ClassItem)env.addClass(i,false,false)).raw());
        }
        _interfaces = JavaConversions.asScalaBuffer(j_interfaces).toList();
      }
      return _interfaces;
    }

    public scala.collection.immutable.List<RefType> supers() {
      if (_supers == null) {
        // It would be nice to just call ::, but I'm not sure how, so we rebuild the whole list.
        ArrayList<RefType> supers = new ArrayList<RefType>();
        supers.add(base());
        scala.collection.immutable.List<ClassType> i = interfaces();
        while (!i.isEmpty()) {
          supers.add(i.head());
          i = (scala.collection.immutable.List<ClassType>)i.tail();
        }
        _supers = JavaConversions.asScalaBuffer(supers).toList();
      }
      return _supers;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean canEqual(Object x) { return this == x; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  TypeItem addClass(PsiClass cls, boolean recurse, boolean noProtected) {
    {
      Item i = lookupNull(cls);
      if (i != null)
        return (TypeItem)i;
    }
    if (cls instanceof PsiTypeParameter)
      return addTypeParam((PsiTypeParameter)cls);

    Item ciBase = tarski.Tarski.baseLookupJava(cls.getQualifiedName());
    if (ciBase != null) {
      locals.put(cls,ciBase);
      return (TypeItem)ciBase;
    }

    PsiElement celem = place.containing(cls);
    ParentItem parent = celem != null ? (ParentItem) addContainer(celem) : tarski.Tarski.localPkg();

    // Type parameters
    scala.collection.immutable.List<TypeVar> tparams = this.tparams(cls);

    // Make and add the class
    ClassItem item = new LazyClass(this,cls,parent);
    locals.put(cls,item);

    // Make sure type parameters are traversed
    item.tparams();

    // Add subthings recursively
    if (recurse) {
      for (PsiField f : cls.getFields())
        if (!place.isInaccessible(f,noProtected))
          addField(f);
      for (PsiMethod m : cls.getMethods())
        if (!place.isInaccessible(m,noProtected))
          addMethod(m);
      for (PsiMethod m : cls.getConstructors())
        if (!place.isInaccessible(m,noProtected))
          addMethod(m);
      for (PsiClass c : cls.getInnerClasses())
        if (!place.isInaccessible(c,noProtected))
          addClass(c,true,noProtected);
    }
    return item;
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

  private static class LazyConstructor extends ConstructorItem {
    private final Converter env;
    private final PsiMethod method;
    private final scala.collection.immutable.List<TypeVar> _tparams;

    // Lazy fields (null initially)
    private ClassItem _parent;
    private scala.collection.immutable.List<Type> _params;

    LazyConstructor(Converter env, PsiMethod method, scala.collection.immutable.List<TypeVar> tparams) {
      this.env = env;
      this.method = method;
      this._tparams = tparams;
    }

    // Core interface
    public ClassItem parent() {
      if (_parent == null)
        _parent = (ClassItem)env.addClass(method.getContainingClass(),false,false);
      return _parent;
    }
    public scala.collection.immutable.List<TypeVar> tparams() {
      return _tparams;
    }
    public scala.collection.immutable.List<Type> params() {
      if (_params == null)
        _params = env.params(method);
      return _params;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean canEqual(Object x) { return this == x; }
    public Parent simple() { throw new RuntimeException("For ConstructorItem, only inside is valid, not simple"); }
    public Option<Parent> safe() { return new Some<Parent>(this); }
    public scala.collection.immutable.Map<TypeVar,Option<RefType>> env() { return Map$.MODULE$.empty(); }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  private static class LazyMethod extends SMethodItem {
    final Converter env;
    final PsiMethod method;
    private final scala.collection.immutable.List<TypeVar> _tparams;
    private final boolean _isStatic;

    // Lazy fields (null initially)
    private ClassItem _parent;
    private scala.collection.immutable.List<Type> _params;
    private Type _retVal;

    LazyMethod(Converter env, PsiMethod method, scala.collection.immutable.List<TypeVar> tparams) {
      this.env = env;
      this.method = method;
      this._tparams = tparams;
      this._isStatic = method.hasModifierProperty(PsiModifier.STATIC);
    }

    // Core interface
    public String name() {
      return method.getName();
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
      return _tparams;
    }
    public scala.collection.immutable.List<Type> params() {
      if (_params == null)
        _params = env.params(method);
      return _params;
    }
    public Type retVal() {
      if (_retVal == null)
        _retVal = env.convertType(method.getReturnType());
      return _retVal;
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean canEqual(Object x) { return this == x; }
    public Parent simple() { throw new RuntimeException("For ConstructorItem, only inside is valid, not simple"); }
    public Option<Parent> safe() { return new Some<Parent>(this); }
    public scala.collection.immutable.Map<TypeVar,Option<RefType>> env() { return Map$.MODULE$.empty(); }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  CallableItem addMethod(PsiMethod method) {
    {
      Item i = lookupNull(method);
      if (i != null)
        return (CallableItem)i;
    }

    // Add type parameters
    List<TypeVar> jtparams = new ArrayList<TypeVar>();
    for (PsiTypeParameter tp : method.getTypeParameters())
      jtparams.add(addTypeParam(tp));
    scala.collection.immutable.List<TypeVar> tparams = scala.collection.JavaConversions.asScalaBuffer(jtparams).toList();

    ClassItem cls = (ClassItem)addClass(method.getContainingClass(),false,false);
    Option<String> qual = tarski.Items.memberQualifiedName(cls,method.getName());
    Item base = qual.isEmpty() ? null : tarski.Tarski.baseLookupJava(qual.get());
    CallableItem item = base != null                                   ? (CallableItem)base
                      : method.isConstructor()                         ? new LazyConstructor(this,method,tparams)
                                                                       : new LazyMethod(this,method,tparams);
    locals.put(method,item);
    return item;
  }

  private static class LazyField extends FieldItem {
    private final Converter env;
    private final PsiField f;
    private final boolean _isFinal;

    // Lazy fields
    private Type _inside;

    LazyField(Converter env, PsiField f, boolean isFinal) {
      this.env = env;
      this.f = f;
      this._isFinal = isFinal;
    }

    public String name() {
      return f.getName();
    }
    public boolean isFinal() {
      return _isFinal;
    }
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

    // Necessary only due to screwy Java/Scala interop
    public boolean canEqual(Object x) { return this == x; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  private static class LazyStaticField extends StaticFieldItem {
    private final Converter env;
    private final PsiField f;
    private final boolean _isFinal;

    // Lazy fields
    private Type _ty;

    LazyStaticField(Converter env, PsiField f, boolean isFinal) {
      this.env = env;
      this.f = f;
      this._isFinal = isFinal;
    }

    public String name() {
      return f.getName();
    }
    public boolean isFinal() {
      return _isFinal;
    }
    public Type ty() {
      if (_ty == null)
        _ty = env.convertType(f.getType());
      return _ty;
    }
    public ClassItem parent() {
      return (ClassItem)env.addClass(f.getContainingClass(),false,false);
    }

    // Necessary only due to screwy Java/Scala interop
    public boolean canEqual(Object x) { return this == x; }
    public int productArity() { throw new NotImplementedError("Should never happen"); }
    public Object productElement(int i) { throw new NotImplementedError("Should never happen"); }
  }

  Value addField(PsiField f) {
    {
      Item i = lookupNull(f);
      if (i != null)
        return (Value)i;
    }

    Value v;
    if (f instanceof PsiEnumConstant) {
      ClassItem c = (ClassItem)addClass(f.getContainingClass(),false,false);
      v = new EnumConstantItem(f.getName(),c);
    } else {
      boolean isFinal = f.hasModifierProperty(PsiModifier.FINAL);
      v = (f.hasModifierProperty(PsiModifier.STATIC) ? new LazyStaticField(this,f,isFinal)
                                                     : new LazyField(this,f,isFinal));
    }
    locals.put(f,v);
    return v;
  }
}
