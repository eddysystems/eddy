package tarski

import tarski.Environment.PlaceInfo
import utility.Utility._
import tarski.AST._
import tarski.Base._
import tarski.Denotations.{PackageDen, Lit}
import tarski.Pretty._
import tarski.Tokens._
import tarski.Types._
import scala.annotation.tailrec
import scala.language.implicitConversions

object Items {

    class ArityMismatchException(name: String, wanted: Int, got: List[TypeArg]) extends RuntimeException(s"Arity mismatch: $name takes $wanted argument${if (wanted==1) "" else "s"}, not ${got.size} ($got)") {}
    class ParentMismatchException(name: String, wanted: ParentItem, got: Parent) extends RuntimeException(s"Parent mismatch: $name expected $wanted, got $got") {}

    // A language item, given to us by someone who knows about the surrounding code
    sealed trait Item extends RefEq with Tries.Named with Tries.Delable {
      def name: Name
      def qualified: Name = name // Overridden by Member

      // false if access restricted by private or protected. lazy classes from the environment override this
      def accessible(place: PlaceInfo) = true
    }

    // Something which we can be inside
    sealed trait ParentItem extends Item with PackageOrMember {
      def inside: Parent
      def raw: Parent
      def simple: Parent
    }
    sealed trait SimpleParentItem extends ParentItem with SimpleParent {
      def item = this
      def inside = this
    }

    abstract class UnknownContainerItemBase extends SimpleParentItem {
      def pkg: Package
    }

    // Containing package
    sealed trait PackageOrMember extends Item
    @tailrec def pkg(x: PackageOrMember): Package = x match {
      case x:Package => x
      case x:Member => pkg(x.parent)
      case x:UnknownContainerItemBase => x.pkg
    }
    // Are we inside a class?
    @tailrec def inClass(x: PackageOrMember, c: ClassItem): Boolean = x==c || (x match {
      case _:Package => false
      case x:Member => inClass(x.parent,c)
      case _:UnknownContainerItemBase => false
    })
    // Are we inside a package?
    @tailrec def inPackage(x: Item, p: Package): Boolean = x==p || (x match {
      case x:Member => inPackage(x.parent,p)
      case _ => false
    })

    // Type parameters.  Must be abstract for lazy generation of fresh variables (which can be recursive).
    case class NormalTypeVar(name: String, base: RefType, interfaces: List[ClassType]) extends TypeVar {
      override def supers = base :: interfaces
      val superItems = supers map (_.item)
      def lo = NullType
      val hi = glb(supers)
      def isFresh = false
    }
    case class SimpleTypeVar(name: String) extends TypeVar {
      def superItems = List(ObjectItem)
      def lo = NullType
      def hi = ObjectType
      def isFresh = false
    }

    // Packages
    sealed abstract class Package extends Item with SimpleParentItem with PackageDen {
      def p = this
      def simple = this
      override def toString = s"Package($qualified)"
    }
    case class RootPackage(name: Name) extends Package
    case class ChildPackage(parent: Package, name: Name) extends Package with Member
    case object LocalPkg extends Package {
      def name = ""
      override def toString = "LocalPkg"
    }
    object Package {
      def apply(names: Name*): Package = names.toList match {
        case Nil => LocalPkg
        case n::ns => ns.foldLeft(RootPackage(n):Package)(ChildPackage)
      }
    }

    // Types
    sealed trait TypeItem extends Item {
      def supers: List[RefType]
      def superItems: List[RefTypeItem] // supers map (_.item), but faster
      def inside: Type
      def raw: Type
      def simple: Type
    }
    abstract class LangTypeItem extends TypeItem {
      def ty: LangType
      val name = show(ty)
      def supers = Nil
      def superItems = Nil
      def inside = ty
      def raw = ty
      def simple = ty
    }

    trait GenericItem {
      def tparams: List[TypeVar]
      def arity: Int = tparams.size
    }

    trait RefTypeItem extends TypeItem { // Not sealed so that TypeVar can inherit from here
      def inside: RefType
      def raw: RefType
      def simple: RefType
    }

    abstract class ClassItem extends RefTypeItem with ParentItem with Member with GenericItem {
      def parent: ParentItem
      def isClass: Boolean // true for class, false for interface
      def isEnum: Boolean // true only for descendants of Enum<E>
      def isFinal: Boolean
      def base: ClassType
      def supers: List[RefType]

      // Can we unbox to a primitive type?
      def unbox: Option[PrimType] = None
      def unboxNumeric: Option[NumType] = None
      def unboxIntegral: Option[IntegralType] = None
      def unboxesToNumeric: Boolean = false
      def unboxesToBoolean: Boolean = false

      // Convert to the type valid inside the definition
      def inside: ClassType = {
        val p = parent.inside
        if (arity == 0) SimpleType(this,p)
        else GenericType(this,tparams,p)
      }

      // Convert to a type valid anywhere, bailing if type parameters are required
      def simple: ClassType =
        if (arity == 0) SimpleType(this,parent.simple)
        else throw new RuntimeException(s"class $name isn't simple (has args $tparams)")

      // Convert to a simple or raw type (valid anywhere)
      def raw: ClassType = {
        def p = parent.raw
        if (arity == 0) SimpleType(this,p)
        else RawType(this,p)
      }

      // Convert to a type valid anywhere
      def generic(args: List[TypeArg], par: Parent): ClassType = {
        if (par.item != parent)
          throw new ParentMismatchException(name, parent, par)
        if (arity != args.size)
          throw new ArityMismatchException(name, arity, args)
        if (arity == 0) SimpleType(this,par)
        else GenericType(this,args,par)
      }

      def generic(args: List[TypeArg]): ClassType = generic(args,parent.simple)

      // true if this class (not its supers) declare a field with this name
      def declaresField(kid: Name): Boolean

      // All constructors of this class
      def constructors: Array[ConstructorItem]

      // All constructors visible from the given place
      def constructors(place: PlaceInfo): Array[ConstructorItem] = constructors filter (_.accessible(place))
    }

    private val noConstructors: Array[ConstructorItem] = Array()
    trait BaseItem extends ClassItem {
      val fieldNames: java.util.Set[String] = new java.util.HashSet[String]()
      var constructors: Array[ConstructorItem] = noConstructors
      def declaresField(kid: Name) = fieldNames.contains(kid)
    }

    case object ObjectItem extends BaseItem {
      def name = "Object"
      def parent = JavaLangPkg
      def isClass = true
      def isEnum = false
      def isFinal = false
      def tparams = Nil
      def base = throw new RuntimeException("Object has no base")
      def supers = Nil
      def superItems = Nil
      override val inside = ObjectType
      override def simple = ObjectType
      override def raw = ObjectType
      override def generic(args: List[TypeArg], par: Parent) = {
        if (par.item != parent) throw new RuntimeException(s"parent mismatch: expected $parent, got $par}")
        if (args.nonEmpty) throw new RuntimeException("Object takes no arguments")
        ObjectType
      }
    }

    class NormalInterfaceItem(val name: Name, val parent: ParentItem, val tparams: List[TypeVar] = Nil,
                              val interfaces: List[ClassType] = Nil, val fields: Set[String] = Set(),
                              _constructors: => Array[ConstructorItem] = noConstructors) extends ClassItem {
      def base = ObjectType
      def supers = if (interfaces.isEmpty) base :: interfaces else interfaces
      def superItems = supers map (_.item)
      def isClass = false
      def isEnum = false
      def isFinal = false
      def declaresField(kid: Name) = fields contains kid
      lazy val constructors = _constructors

      override def toString = {
        def f[A](s: String, x: A, d: A) = if (x == d) "" else s",$s=$x"
        (s"NormalInterfaceItem($name"
          + (if (parent==LocalPkg) "" else s",$parent")
          + f("tparams",tparams,Nil)
          + f("interfaces",interfaces,Nil)
          + ')')
      }
    }
    object NormalInterfaceItem {
      def apply(name: Name, parent: ParentItem = LocalPkg, tparams: List[TypeVar] = Nil,
                interfaces: List[ClassType] = Nil, fields: Set[String] = Set(),
                constructors: => Array[ConstructorItem] = noConstructors): ClassItem =
        new NormalInterfaceItem(name,parent,tparams,interfaces,fields,constructors)
    }

    class NormalClassItem(val name: Name, val parent: ParentItem = LocalPkg, val tparams: List[TypeVar] = Nil,
                          val base: ClassType = ObjectType, val interfaces: List[ClassType] = Nil,
                          val isFinal: Boolean = false, val fields: Set[String] = Set(),
                          _constructors: => Array[ConstructorItem] = noConstructors) extends ClassItem {
      def supers = base :: interfaces
      def superItems = supers map (_.item)
      def isClass = true
      def isEnum = false
      def declaresField(kid: Name) = fields contains kid
      lazy val constructors = _constructors

      override def toString = {
        def f[A](s: String, x: A, d: A) = if (x == d) "" else s",$s=$x"
        (s"NormalClassItem($name"
          + (if (parent==LocalPkg) "" else s",$parent")
          + f("tparams",tparams,Nil)
          + f("base",base,ObjectType)
          + f("interfaces",interfaces,Nil)
          + f("isFinal",isFinal,false)
          + ')')
      }
    }
    object NormalClassItem {
      def apply(name: Name, parent: ParentItem = LocalPkg, tparams: List[TypeVar] = Nil,
                base: ClassType = ObjectType, interfaces: List[ClassType] = Nil,
                isFinal: Boolean = false, fields: Set[String] = Set(),
                constructors: => Array[ConstructorItem] = noConstructors): ClassItem =
        new NormalClassItem(name,parent,tparams,base,interfaces,isFinal,fields,constructors)
    }

    case object ArrayItem extends RefTypeItem {
      def name = "Array"
      def parent = JavaLangPkg
      private def error = throw new RuntimeException("Array<T> is special: T can be primitive, and is covariant")
      def tparams = error
      val superItems = List(SerializableItem,CloneableItem)
      val supers = superItems map (_.simple)
      def inside = error
      def raw = error
      def simple = error
      def generic(args: List[TypeArg], parent: Parent) = error
    }
    case object NoTypeItem extends RefTypeItem {
      def name = "NoTypeItem"
      def supers = Nil
      def superItems = Nil
      private def error = throw new RuntimeException("NoTypeItem shouldn't be touched")
      def inside = error
      def raw = error
      def simple = error
    }

    trait Member extends Item with PackageOrMember {
      def name: Name
      def parent: ParentItem // Package, class, or callable
      override def qualified = parent.qualified + "." + name
    }
    trait ClassMember extends Member {
      def parent: ClassItem
    }

    // Values
    sealed abstract class Value extends Item {
      def item: TypeItem // The item of our type
      def isFinal: Boolean
    }
    abstract class Local extends Value {
      override def toString = (if (isParameter) "param:" else "local:") + name
      def ty: Type
      def item = ty.item
      def isParameter: Boolean // true if parameter, false if local variable
    }
    case class NormalLocal(name: Name, ty: Type, isParameter: Boolean = false, isFinal: Boolean = true) extends Local {}
    case class ThisItem(self: ClassItem) extends Value with PseudoCallableItem {
      def name = "this"
      def item = self
      def inside = self.inside
      def isFinal = true
    }
    case class SuperItem(self: ClassType) extends Value with PseudoCallableItem {
      def name = "super"
      def item = self.item
      def inside = self
      def isFinal = true
    }
    abstract class FieldItem extends Value with ClassMember {
      def inside: Type
      def item = inside.item
      def isStatic: Boolean
    }
    case class LitValue(x: Lit) extends Value {
      val name = show(x)
      val ty = x.ty
      val item = x.item
      def isFinal = true
    }

    // Normal values
    case class NormalFieldItem(name: Name, inside: Type, parent: ClassItem, isFinal: Boolean) extends FieldItem {
      val isStatic = false
    }
    case class NormalStaticFieldItem(name: Name, ty: Type, parent: ClassItem, isFinal: Boolean) extends FieldItem {
      val isStatic = true
      def inside: Type = ty
    }

    // Callables
    sealed trait PseudoCallableItem extends Item // MethodItem or this or super
    sealed abstract class CallableItem extends SimpleParentItem with GenericItem with ClassMember {
      def parent: ClassItem
      def params: List[Type]
      def simple: Parent = throw new RuntimeException("For CallableParentItem, only inside is valid, not simple")
    }
    abstract class MethodItem extends CallableItem with PseudoCallableItem {
      def retVal: Type
      def isStatic: Boolean
    }
    abstract class ConstructorItem extends CallableItem {
      def name = parent.name
    }

    // Normal callables
    case class NormalMethodItem(name: Name, parent: ClassItem, tparams: List[TypeVar], retVal: Type,
                                params: List[Type], isStatic: Boolean) extends MethodItem
    case class NormalConstructorItem(parent: ClassItem, tparams: List[TypeVar],
                                     params: List[Type]) extends ConstructorItem {
      override def toString = s"NormalConstructorItem(${parent.name},$tparams,$params)"
    }
    case class DefaultConstructorItem(parent: ClassItem) extends ConstructorItem {
      val tparams = Nil
      val params = Nil
    }

    // Labels
    case class Label(name: Name, continuable: Boolean) extends Item

    // traits used by lazy classes that can change some of their fields
    trait CachedConstructorsItem {
      def invalidateConstructors(): Unit
    }

    trait CachedTypeParametersItem {
      def invalidateTypeParameters(): Unit
    }

    trait CachedBaseItem {
      def invalidateBase(): Unit
    }

    trait CachedSupersItem {
      def invalidateSupers(): Unit
    }

    trait CachedReturnTypeItem {
      def invalidateReturnType(): Unit
    }

    trait CachedParametersItem {
      def invalidateParameters(): Unit
    }

    trait SettableFinalItem {
      def setFinal(f: Boolean): Unit
    }

    trait SettableStaticItem {
      def setStatic(f: Boolean): Unit
    }

    trait CachedNameItem {
      def refreshName(): Unit
    }
  }
