package com.eddysystems.eddy.engine;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiClassReferenceType;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
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
   final GlobalSearchScope projectScope;

   final JavaEnvironment jenv;

   // non-project items go here
   protected final Map<PsiElement, Item> globals;

   // project items go here
   protected final Map<PsiElement, Item> locals;

   // anything added to project items also goes in here
   protected final Map<PsiElement, Item> added;

   // true if we should sort items into globals and locals, false if we put everything in locals
   private final boolean splitScope;

   Converter(JavaEnvironment jenv,
             final Map<PsiElement, Item> globals,
             final Map<PsiElement, Item> locals,
             final Map<PsiElement, Item> added) {
     this.project = jenv.project;
     this.projectScope = ProjectScope.getProjectScope(project);
     this.jenv = jenv;
     this.globals = globals;
     this.locals = locals;
     this.added = added;
     this.splitScope = true;
   }

   // constructed like this, put and lookup behave differently
   Converter(JavaEnvironment jenv,
             final Map<PsiElement, Item> scope) {
     this.project = jenv.project;
     this.projectScope = ProjectScope.getProjectScope(project);
     this.jenv = jenv;
     this.globals = null;
     this.locals = scope;
     this.added = null;
     this.splitScope = false;
   }

   Item lookup(PsiElement e) {
     if (globals == null) {
       // if globals is null, we're ok with referencing local items
       return jenv.lookup(e, true);
     } else {
       // if we're the global converter, and we find a local thing, move it to the global part of the environment, it's
       // clearly needed there more than here
       return jenv.lookupAndMove(e);
     }
   }

   void put(PsiElement e, Item it) {
     if (splitScope) {
       if (isInProject(e)) {
         synchronized (jenv) {
           locals.put(e,it);
           // don't add constructor items to added -- they won't end up in the trie anyway
           if (!(it instanceof ConstructorItem) && added != null)
             added.put(e,it);
         }
       } else {
         synchronized (jenv) {
           globals.put(e,it);
         }
       }
     } else {
       synchronized (jenv) {
         locals.put(e,it);
         if (!(it instanceof ConstructorItem) && added != null)
           added.put(e,it);
       }
     }
   }

   boolean isInProject(PsiElement elem) {
     if (elem instanceof PsiPackage) {
       VirtualFile[] files = ((PsiPackage) elem).occursInPackagePrefixes();
       for (VirtualFile file : files) {
         if (projectScope.contains(file))
           return true;
       }
       return false;
     } else {
       VirtualFile vf = Place.getElementFile(elem);
       if (vf != null)
         return projectScope.contains(vf);
       else {
         //log("can't determine whether " + elem + " is in project.");
         return true;
       }
     }
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

         ClassItem item = (ClassItem)addClass(tcls,false);

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
     synchronized (jenv) {
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
         return (ParentItem)addClass((PsiClass) elem, false);
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

   static protected class UnknownContainerItem extends UnknownContainerItemBase implements PsiEquivalent, CachedNameItem {

     final PsiElement elem;
     final Converter converter;

     UnknownContainerItem(PsiElement elem, Converter converter) {
       this.elem = elem;
       this.converter = converter;
     }

     @Override
     public void refreshName() {}

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

   static protected class UnresolvedClassItem extends ClassItem implements PsiEquivalent, CachedNameItem, SettableFinalItem, SettableStaticItem {
     @NotNull final Converter env;
     @NotNull final PsiClassReferenceType cls;

     @NotNull final Parent _parent;
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

     public void refreshName() { /* not caching */ }

     public PsiElement psi() {
        return cls.getReference();
      }

     public void setFinal(boolean f) {
        _isFinal = f;
     }

     public void setStatic(boolean f) {
        _isStatic = f;
      }
   }


   protected static class LazyTypeVar extends TypeVar implements PsiEquivalent, ReferencingItem, CachedNameItem, CachedSupersItem {
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
           supers.add(env.addClass(c,false));
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

     Items.ParentItem _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.place() != _place) {
         _place = info.place();
         if (info.place() instanceof PsiEquivalent && p.getOwner() != null) {
           // Type variables are visible exactly if we are inside their owner
           _accessible =  PsiTreeUtil.isAncestor(p.getOwner(), ((PsiEquivalent) _place).psi(), false);
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
     synchronized (jenv) {
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
   }

   static protected class LazyClass extends ClassItem implements PsiEquivalent, ReferencingItem, CachedNameItem, CachedConstructorsItem, CachedTypeParametersItem, CachedBaseItem, CachedSupersItem, SettableFinalItem, SettableStaticItem {
     private final Converter env;
     private final PsiClass cls;
     private final ParentItem _parent;
     private boolean _isFinal;
     private boolean _isStatic;
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
               _base = ((ClassItem)env.addClass(base,false)).inside();
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
           supers.add(env.addClass(s, false));
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

     public void setStatic(boolean f) { _isStatic = f; }

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

     Items.ParentItem _place = null;
     boolean _accessible = false;
     @Override
     public boolean accessible(Environment.PlaceInfo info) {
       if (info.place() != _place) {
         _place = info.place();
         if (info.place() instanceof PsiEquivalent) {
           _accessible = !new Place(env.project, ((PsiEquivalent) _place).psi()).isInaccessible(cls);
         } else {
           log("can't determine whether " + this + " is accessible from " + info.place());
           _accessible = false;
         }
       }
       return _accessible;
     }
   }

   // used to dynamically add items from Psi tree change events
   Item addItem(PsiElement elem) {
     if (elem instanceof PsiClass) {
       return addClass((PsiClass)elem, true);
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

   RefTypeItem addClass(PsiClass cls, boolean recurse) {
     synchronized(jenv) {
       {
         Item i = lookup(cls);
         if (i != null) {
           // if we are forced to recurse, we may still have to add fields that were previously unseen
           // don't do this for TypeVars
           if (recurse && i instanceof ClassItem)
             addClassMembers(cls, (ClassItem)i);
           return (RefTypeItem)i;
         }
       }

       if (cls instanceof PsiTypeParameter)
         return addTypeParam((PsiTypeParameter)cls);

       // Make and add the class
       ParentItem parent;
       PsiElement cont = Place.containing(cls, project);
       try {
         parent = addContainer(cont);
       } catch (UnknownContainerError e) {
         log(e);
         parent = new UnknownContainerItem(cont, this);
       }

       ClassItem item = new LazyClass(this,cls,parent);
       put(cls, item);

       // if we're a class, all our members should really be ok
       if (recurse)
         addClassMembers(cls,item);
       return item;
     }
   }

   void addClassMembers(PsiClass cls, ClassItem item) {
     final Set<String> set = item instanceof BaseItem ? ((BaseItem)item).fieldNames() : null;
     for (PsiField f : cls.getFields()) {
       if (set != null)
         set.add(f.getName());
       addField(f);
     }
     for (PsiMethod m : cls.getMethods())
       addMethod(m);

     for (PsiClass c : cls.getInnerClasses())
       addClass(c,true);
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

     public String toString() { return "LazyConstructor:" + name(); }

     // Core interface
     public ClassItem parent() {
       if (_parent == null)
         _parent = (ClassItem)env.addClass(method.getContainingClass(),false);
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
         _parent = (ClassItem)env.addClass(method.getContainingClass(),false);
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
     synchronized (jenv) {
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
            return (ClassItem)env.addClass(f.getContainingClass(),false);
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
    synchronized (jenv) {
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
  }

  protected static class LazyLocal extends Local implements PsiEquivalent, ReferencingItem, CachedNameItem, SettableFinalItem {
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
    public void setFinal(boolean b) { _isFinal = b; }
    public void refreshName() { _name = var.getName(); }

    public boolean flushReference(Item it) {
      if (_ty != null && _ty.item() == it)
        _ty = null;
      return false;
    }

    public boolean fillUnresolvedReferences() {
      if (_ty != null && _ty instanceof RefType)
        _ty = checkReplaceUnresolved((RefType)_ty);
      return false;
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
    synchronized (jenv) {
      assert globals == null; // only the local converter should be used like this
      assert added == null;
      assert var instanceof PsiLocalVariable || var instanceof PsiParameter;

      // lookup only in locals (
      Item i = locals.get(var);
      if (i != null)
        return (Value)i;
      else
        // make sure Values never end up anywhere outside locals
        assert !jenv.knows(var);

      // add only to locals
      final boolean isFinal = var.hasModifierProperty(PsiModifier.FINAL);
      i = (Items.Item) new LazyLocal(this, var, isFinal);
      locals.put(var, i);
      return (Value)i;
    }
  }
}
