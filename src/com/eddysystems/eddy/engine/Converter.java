package com.eddysystems.eddy.engine;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiClassReferenceType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.NotImplementedError;
import scala.Some;
import scala.collection.JavaConversions;
import tarski.Environment;
import tarski.Items;
import tarski.Items.*;
import tarski.Types.*;
import utility.JavaUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.eddysystems.eddy.engine.Utility.log;
import static utility.JavaUtils.scalaList;

class Converter {
   public final Project project;

   // cached items
   protected final Map<PsiElement, Item> items;

   Converter(Project project, final Map<PsiElement, Item> items) {
     this.project = project;
     this.items = items;
   }

   boolean knows(PsiElement e) {
     return items.containsKey(e);
   }

   Item lookup(PsiElement e) {
     return items.get(e);
   }

   void put(PsiElement e, Item it) {
     items.put(e,it);
   }

   // return type is null *and* has same name as containing class
   static boolean isConstructor(PsiMethod elem) {
     // elem.isConstructor checks whether the method has a null return type element
     return elem.isConstructor() && elem.getContainingClass() != null && elem.getName().equals(elem.getContainingClass().getName());
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
       PsiClass tcls = null;
       try {
         tcls = ((PsiClassType)t).resolve();
       } catch (PsiInvalidElementAccessException e) {
         // this happens in scala code, which doesn't assign proper files to synthetic elements -- ignore
       }
       if (tcls == null) {
         // we don't know how to deal with unresolved things that are not references.
         assert t instanceof PsiClassReferenceType;
         // ClassType cannot be resolved to a Class (probably because its class file is missing or the code is incomplete
         // or there is an error)
         return new UnresolvedClassItem(this, (PsiClassReferenceType)t, parent).generic();
       } else if (tcls instanceof PsiTypeParameter) {
         return addTypeParam((PsiTypeParameter) tcls);
       } else {
         List<TypeArg> jparams = new SmartList<TypeArg>();
         for (PsiType tp : ((PsiClassType)t).getParameters())
           jparams.add(convertTypeArg(tp,parent));
         scala.collection.immutable.List<TypeArg> params = scala.collection.JavaConversions.asScalaBuffer(jparams).toList();

         ClassItem item = (ClassItem)addClass(tcls);

         assert params.size() == ((PsiClassType)t).getParameterCount();

         if (item.arity() > 0 && params.isEmpty()) {
           return item.raw();
         } else if (parent == null) {
           ParentItem container = addContainer(Place.containing(tcls, project));
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

   static class UnknownContainerError extends RuntimeException {
     UnknownContainerError(PsiElement elem) {
        super("Unknown container type: " + elem);
      }
   }

   ParentItem addContainer(PsiElement elem) {
     {
       Item i = lookup(elem);
       if (i != null)
         return (ParentItem)i;
     }
     if (elem == null)
       return LocalPkg$.MODULE$;

     // local classes
     if (elem instanceof PsiMethod) {
       return addMethod((PsiMethod) elem);
     } if (elem instanceof PsiClass)
       return (ParentItem)addClass((PsiClass) elem);
     else if (elem instanceof PsiPackage) {
       final PsiPackage pkg = (PsiPackage)elem;
       final String name = pkg.getName();
       if (name == null)
         return LocalPkg$.MODULE$;
       final PsiPackage parent = pkg.getParentPackage();
       final ParentItem item = parent==null ? new RootPackage(name)
         : new ChildPackage((tarski.Items.Package)addContainer(parent),name);
       put(pkg,item);
       return item;
     }
     throw new UnknownContainerError(elem);
   }

   protected interface PsiEquivalent {
     PsiElement psi();
   }

   static protected class UnknownContainerItem extends UnknownContainerItemBase implements PsiEquivalent {

     final PsiElement elem;
     final Converter converter;

     UnknownContainerItem(PsiElement elem, Converter converter) {
       this.elem = elem;
       this.converter = converter;
     }

     @Override
     public Parent simple() { throw new RuntimeException("For UnknownContainerItem, only inside is valid, not simple"); }

     @Override
     public String name() {
       return elem.getText();
     }

     @Override
     public PsiElement psi() {
        return elem;
      }

     @Override
     public Items.Package pkg() { return (Items.Package)converter.addContainer(Place.getElementPackage(elem, converter.project)); }
   }

   static protected class UnresolvedClassItem extends ClassItem implements PsiEquivalent {
     @NotNull final Converter env;
     @NotNull final PsiClassReferenceType cls;

     @NotNull Parent _parent;
     boolean _isFinal = false;
     boolean _isStatic = false;
     scala.collection.immutable.List<TypeArg> _targs;
     scala.collection.immutable.List<TypeVar> _tparams = null;
     ClassItem _resolved = null;

     static final ConstructorItem[] noConstructors = new ConstructorItem[0];
     static final scala.collection.immutable.List<RefType> _supers = scalaList((RefType) ObjectType$.MODULE$);
     static final scala.collection.immutable.List<RefTypeItem> _superItems = scalaList((RefTypeItem) ObjectItem$.MODULE$);

     UnresolvedClassItem(@NotNull final Converter env, @NotNull final PsiClassReferenceType cls, @Nullable final Parent parent) {
       this.env = env;
       this.cls = cls;
       PsiPackage pkg = Place.getElementPackage(cls.getReference(), env.project);
       _parent = parent == null ? pkg == null ? LocalPkg$.MODULE$ : (tarski.Items.Package)env.addContainer(pkg) : parent;

       if (cls instanceof PsiModifierListOwner) {
         _isFinal = ((PsiModifierListOwner)cls).hasModifierProperty(PsiModifier.FINAL);
         _isStatic = Place.isStatic((PsiModifierListOwner) cls);
       }

       List<TypeArg> jargs = new SmartList<TypeArg>();
       for (PsiType arg : cls.getParameters())
         jargs.add(env.convertTypeArg(arg,parent));
       _targs = scala.collection.JavaConversions.asScalaBuffer(jargs).toList();
     }

     public String name() { return cls.getClassName(); }
     public boolean isClass() { return true; }
     public boolean isEnum() { return false; }
     public boolean isFinal() { return _isFinal; }
     public boolean isStatic() { return _isStatic; }
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

     public ConstructorItem[] constructors() { return noConstructors; }

     public ConstructorItem[] constructors(Environment.PlaceInfo place) { return noConstructors; }

     public PsiElement psi() {
        return cls.getReference();
      }
   }


   protected static class LazyTypeVar extends TypeVar implements PsiEquivalent {
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

     public boolean isFresh() {
        return false;
      }

     public scala.collection.immutable.List<RefTypeItem> superItems() {
       if (_superItems == null) {
         List<RefTypeItem> supers = new SmartList<RefTypeItem>();
         for (PsiClass c : p.getSupers())
           supers.add(env.addClass(c));
         _superItems = JavaConversions.asScalaBuffer(supers).toList();
       }
       return _superItems;
     }

     public PsiElement psi() {
        return p;
      }

     Items.ParentItem _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.place() != _place) {
         _place = info.place();
         if (info.place() instanceof com.eddysystems.eddy.engine.Converter.PsiEquivalent && p.getOwner() != null) {
           // Type variables are visible exactly if we are inside their owner
           _accessible =  PsiTreeUtil.isAncestor(p.getOwner(), ((com.eddysystems.eddy.engine.Converter.PsiEquivalent) _place).psi(), false);
         } else {
           //log("can't determine whether " + this + " is accessible from " + info.place());
           _accessible = false;
         }
       }
       return _accessible;
     }

     // Necessary only due to screwy Java/Scala interop
     public Some safe() { return new Some<RefType>(this); }
   }

   private TypeVar addTypeParam(PsiTypeParameter p) {
     {
       Item i = lookup(p);
       if (i != null)
         return (TypeVar)i;
     }
     // Use a maker to break recursion
     TypeVar ti = new LazyTypeVar(this,p);
     put(p, ti);
     return ti;
   }

   static protected class LazyClass extends ClassItem implements PsiEquivalent {
     private final Converter env;
     private final PsiClass cls;
     private boolean _isFinal;
     private boolean _isStatic;
     private String _name;

     // Lazy fields
     private ParentItem _parent;
     private scala.collection.immutable.List<TypeVar> _tparams;
     private ClassType _base;
     private scala.collection.immutable.List<RefType> _supers;
     private scala.collection.immutable.List<RefTypeItem> _superItems;
     private ConstructorItem[] _constructors;

     LazyClass(Converter env, PsiClass cls) {
       this.env = env;
       this.cls = cls;
       this._name = cls.getName();
       this._isFinal = cls.hasModifierProperty(PsiModifier.FINAL);
       this._isStatic = Place.isStatic(cls);
     }

     public String toString() { return "LazyClass:" + _name; }

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

     public boolean isStatic() { return _isStatic; }

     public boolean isClass() {
        return !cls.isInterface();
      }

     public boolean isEnum() {
        return cls.isEnum();
      }

     public ParentItem parent() {
       if (_parent == null) {
         PsiElement cont = Place.containing(cls, env.project);
         try {
           _parent = env.addContainer(cont);
         } catch (UnknownContainerError e) {
           log(e);
           _parent = new UnknownContainerItem(cont, env);
         }
       }
       return _parent;
      }

     public ClassType base() {
       if (_base == null)
         supers();
       return _base;
     }

     public scala.collection.immutable.List<RefType> supers() {
       if (_supers == null) {
         if (cls instanceof PsiAnonymousClass) {
           // Anonymous classes must be handled specially
           final PsiAnonymousClass anon = (PsiAnonymousClass)cls;
           final ClassType sup = (ClassType)env.convertType(anon.getBaseClassType());
           _base = sup.item().isClass() ? sup : ObjectType$.MODULE$;
           _supers = JavaUtils.<RefType>scalaList(sup);
         } else {
           ArrayList<RefType> supers = new ArrayList<RefType>();
           for (PsiClassType stype : cls.getSuperTypes()) {
             ClassType sc = (ClassType)env.convertType(stype);
             PsiClass stypeClass = stype.resolve();

             // if the code illegally uses an interface in the extends clause, we assume it meant to use that interface in
             // the implements clause. If more than one class are inherited from, use the first one mentioned as base.
             if (stypeClass != null && !stypeClass.isInterface())
               if (_base == null) // first non-interface in supers is base.
                 _base = sc;
               else // other non-interfaces in supers
                 log("multiple class inheritance in " + this + ": at least " + _base + " and " + stypeClass);
             supers.add(sc);
           }
           if (_base == null) {
             PsiClass base = cls.getSuperClass();
             if (base != null && !base.isInterface())
               _base = ((ClassItem)env.addClass(base)).inside();
             else {
               if (base != null)
                 log("class " + this + " extends interface: " + base);
               // base can be null if JDK is not defined, for Object (should be impossible), and for scala traits
               _base = ObjectType$.MODULE$;
             }
           }
           _supers = JavaConversions.asScalaBuffer(supers).toList();
         }
       }
       return _supers;
     }

     public scala.collection.immutable.List<RefTypeItem> superItems() {
       if (_superItems == null) {
         ArrayList<RefTypeItem> supers = new ArrayList<RefTypeItem>();
         for (PsiClass s : cls.getSupers())
           supers.add(env.addClass(s));
         _superItems = JavaConversions.asScalaBuffer(supers).toList();
       }
       return _superItems;
     }

     public boolean declaresField(String kid) {
        return cls.findFieldByName(kid, false) != null;
      }

     public ConstructorItem[] constructors(Environment.PlaceInfo place) {
       int n = 0;
       for (ConstructorItem c : constructors())
         if (c.accessible(place))
           n++;
       ConstructorItem[] cons = new ConstructorItem[n];
       n = 0;
       for (ConstructorItem c : constructors())
         if (c.accessible(place))
           cons[n++] = c;
       return cons;
     }

     public ConstructorItem[] constructors() {
       if (_constructors == null) {
         final ArrayList<ConstructorItem> cons = new ArrayList<ConstructorItem>();
         for (PsiMethod m : cls.getConstructors())
           if (isConstructor(m)) {
             cons.add((ConstructorItem)env.addMethod(m));
           }

         // add implicit constructor if no others declared
         if (cons.isEmpty()) {
           cons.add(new DefaultConstructorItem(this));
         }

         _constructors = cons.toArray(new ConstructorItem[cons.size()]);
       }
       return _constructors;
     }

     public PsiElement psi() {
        return cls;
      }

     Items.ParentItem _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.place() != _place) {
         _place = info.place();
         if (info.place() instanceof com.eddysystems.eddy.engine.Converter.PsiEquivalent) {
           _accessible = !new Place(env.project, ((com.eddysystems.eddy.engine.Converter.PsiEquivalent) _place).psi()).isInaccessible(cls);
         } else {
           log("can't determine whether " + this + " is accessible from " + info.place());
           _accessible = false;
         }
       }
       return _accessible;
     }
   }

   RefTypeItem addClass(PsiClass cls) {
     return addClassInner(cls, false);
   }

   RefTypeItem addClassRecursively(PsiClass cls) {
     return addClassInner(cls, true);
   }

   private RefTypeItem addClassInner(PsiClass cls, final boolean recurse) {
     {
       Item i = lookup(cls);
       if (i != null) {
         // if we are forced to recurse, we may still have to add fields that were previously unseen
         // don't do this for TypeVars
         if (recurse && i instanceof ClassItem)
           addClassMembers(cls, (ClassItem)i, false);
         return (RefTypeItem)i;
       }
     }

     if (cls instanceof PsiTypeParameter)
       return addTypeParam((PsiTypeParameter)cls);

     ClassItem item = new LazyClass(this,cls);
     put(cls, item);

     // if we're a class, all our members should really be ok
     if (recurse)
       addClassMembers(cls,item,false);
     return item;
   }

   // Returns null if collectConstructors is false, otherwise a list of constructors
   List<ConstructorItem> addClassMembers(final PsiClass cls, final ClassItem item, final boolean collectConstructors) {
     final Set<String> set = item instanceof BaseItem ? ((BaseItem)item).fieldNames() : null;
     for (PsiField f : cls.getFields()) {
       if (set != null)
         set.add(f.getName());
       addField(f);
     }

     final List<ConstructorItem> cons = collectConstructors ? new SmartList<ConstructorItem>() : null;
     for (PsiMethod m : cls.getMethods()) {
       final CallableItem mi = addMethod(m);
       if (collectConstructors && mi instanceof ConstructorItem)
         cons.add((ConstructorItem) mi);
     }

     for (PsiClass c : cls.getInnerClasses())
       addClassRecursively(c);

     return cons;
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

   protected static class LazyConstructor extends ConstructorItem implements PsiEquivalent {
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

     public String toString() { return "LazyConstructor:" + name(); }

     // Core interface
     public ClassItem parent() {
       if (_parent == null)
         _parent = (ClassItem)env.addClass(method.getContainingClass());
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

     public PsiElement psi() {
            return method;
          }

     PsiElement _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.exactPlace() != _place) {
         _place = info.exactPlace();
         _accessible = !new Place(env.project, _place).isInaccessible(method);
       }
       return _accessible;
     }
   }

   protected static class LazyMethod extends MethodItem implements PsiEquivalent {
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

     public String toString() { return "LazyMethod:" + _name; }

     // Core interface
     public String name() {
       return _name;
     }
     public boolean isStatic() {
            return _isStatic;
          }
     public ClassItem parent() {
       if (_parent == null)
         _parent = (ClassItem)env.addClass(method.getContainingClass());
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

     public PsiElement psi() {
       return method;
     }

     PsiElement _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.exactPlace() != _place) {
         _place = info.exactPlace();
         _accessible = !new Place(env.project, _place).isInaccessible(method);
       }
       return _accessible;
     }
   }

   CallableItem addMethod(PsiMethod method) {
     if (isConstructor(method)) {
       // constructors are not stored in locals, but in cons
       ConstructorItem i = (ConstructorItem)lookup(method);
       if (i != null)
         return i;
       i = new LazyConstructor(this,method);
       put(method, i);
       return i;
     } else {
       Item i = lookup(method);
       if (i != null)
         return (CallableItem)i;
       i = new LazyMethod(this,method);
       put(method, i);
       return (CallableItem)i;
     }
   }

   protected static class LazyField extends FieldItem implements PsiEquivalent {
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

     public String toString() { return "LazyField:" + _name; }

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
         } catch (Place.UnexpectedContainerError e) {
           log("LazyField:" + qualified() + " failed: " + e.getMessage());
           throw e;
         }
       }
       return _inside;
     }
     public ClassItem parent() {
            return (ClassItem)env.addClass(f.getContainingClass());
          }

     public PsiElement psi() {
            return f;
          }

     PsiElement _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.exactPlace() != _place) {
         _place = info.exactPlace();
         _accessible = !new Place(env.project, _place).isInaccessible(f);
       }
       return _accessible;
     }
   }

  Value addField(PsiField f) {
    {
      final Item i = lookup(f);
      if (i != null)
        return (Value)i;
    }
    final boolean isFinal = f.hasModifierProperty(PsiModifier.FINAL);
    final boolean isStatic = f.hasModifierProperty(PsiModifier.STATIC);
    final Value v = new LazyField(this,f,isFinal,isStatic);
    put(f, v);
    return v;
  }

  protected static class LazyLocal extends Local implements PsiEquivalent {
    private final Converter env;
    private final PsiVariable var;
    private String _name;
    private boolean _isFinal;

    // Lazy fields
    private Type _ty;

    LazyLocal(Converter env, PsiVariable var, boolean isFinal) {
      this.env = env;
      this.var = var;
      this._name = var.getName();
      this._isFinal = isFinal;
    }

    public String toString() { return "LazyLocal:" + _name; }

    public String name() { return _name; }
    public boolean isFinal() { return _isFinal; }
    public Type ty() {
      if (_ty == null) {
        try {
          _ty = env.convertType(var.getType());
        } catch (Place.UnexpectedContainerError e) {
          log("LazyLocal:" + qualified() + " failed: " + e.getMessage());
          throw e;
        }
      }
      return _ty;
    }

    public PsiElement psi() {
      return var;
    }

    private PsiElement _place = null;
    private boolean _accessible = false;
    @Override
    public boolean accessible(Environment.PlaceInfo info) {
      if (info.exactPlace() != _place) {
        _place = info.exactPlace();
        if (var instanceof PsiParameter)
          _accessible = PsiTreeUtil.isAncestor(((PsiParameter)psi()).getDeclarationScope(), _place, false);
        else {
          // find code block, check if place is inside code block, and after our declaration
          PsiCodeBlock cb = PsiTreeUtil.getParentOfType(psi(), PsiCodeBlock.class, true);
          if (PsiTreeUtil.isAncestor(cb, _place, false)) {
            // we're accessible if this is declared before (the end of) _place
            _accessible = _place.getTextRange().getEndOffset() >= var.getTextRange().getEndOffset();
          } else {
            _accessible = false;
          }
        }
      }
      return _accessible;
    }
  }

  Value addLocal(final PsiVariable var) {
    assert var instanceof PsiLocalVariable || var instanceof PsiParameter;

    // lookup only in locals (
    Item i = items.get(var);
    if (i != null)
      return (Value)i;

    // add only to locals
    final boolean isFinal = var.hasModifierProperty(PsiModifier.FINAL);
    i = (Items.Item) new LazyLocal(this, var, isFinal);
    put(var, i);
    return (Value)i;
  }
}
