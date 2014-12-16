package tarski

import ambiguity.Utility._
import tarski.AST._
import tarski.Base._
import tarski.Denotations.Lit
import tarski.Pretty._
import tarski.Tokens._
import tarski.Types._

object Items {
  // A language item, given to us by someone who knows about the surrounding code
  // inherits from Product => only case things or abstract classes can have this trait without implementing Product
  sealed trait Item extends RefEq with Product with Tries.Named {
    def name: Name
    def qualifiedName: Option[Name] // A name that is valid anywhere
    override def toString: String = qualifiedName getOrElse name
    def print: String = scala.runtime.ScalaRunTime._toString(this)
  }

  // Something which we can be inside
  sealed trait PlaceItem extends Item
  sealed trait ParentItem extends PlaceItem {
    def inside: Parent
    def raw: Parent
    def simple: Parent
  }
  sealed trait SimpleParentItem extends ParentItem with SimpleParent {
    def item = this
    def inside = this
  }

  // Type parameters.  Must be abstract for lazy generation of fresh variables (which can be recursive).
  case class NormalTypeVar(name: String, base: RefType, interfaces: List[ClassType]) extends TypeVar {
    override def supers = base :: interfaces
    val superItems = supers map (_.item)
    def lo = NullType
    val hi = glb(supers)
  }
  case class SimpleTypeVar(name: String) extends TypeVar {
    def superItems = List(ObjectItem)
    def lo = NullType
    def hi = ObjectType
  }

  // Packages
  case class PackageItem(name: Name, qualified: Name) extends Item with SimpleParentItem {
    def qualifiedName = Some(qualified)
    def simple = this
  }

  // Annotations
  case class AnnotationItem(name: Name, qualified: Name) extends Item {
    def qualifiedName = Some(qualified)
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
    val name = show(pretty(ty))
    def qualifiedName = Some(name)
    def supers = Nil
    def superItems = Nil
    def inside = ty
    def raw = ty
    def simple = ty

    // Not sure why we need these
    def canEqual(x: Any) = x match { case x:AnyRef => this eq x; case _ => false }
    def productArity = notImplemented
    def productElement(n: Int) = notImplemented
  }

  trait GenericItem {
    def tparams: List[TypeVar]
    def arity: Int = tparams.size
  }

  trait RefTypeItem extends TypeItem // Not sealed so that TypeVar can inherit from here

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
        throw new RuntimeException(s"parent mismatch: expected $parent, got $par}")
      if (arity != args.size)
        throw new RuntimeException(s"arity mismatch: $name takes $arity arguments, not ${args.size} ($args)")
      if (arity == 0) SimpleType(this,par)
      else GenericType(this,args,par)
    }

    def generic(args: List[TypeArg]): ClassType = generic(args,parent.simple)

    // true if this class (not its supers) declare a field with this name
    def declaresField(kid: Name): Boolean

    // All constructors of this class
    def constructors: Array[ConstructorItem]
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
    def supers = base :: interfaces
    def superItems = supers map (_.item)
    def isClass = false
    def isEnum = false
    def isFinal = false
    def declaresField(kid: Name) = fields contains kid
    lazy val constructors = _constructors

    // Not sure why we need these
    def canEqual(x: Any) = x match { case x:AnyRef => this eq x; case _ => false }
    def productArity = notImplemented
    def productElement(n: Int) = notImplemented
  }
  object NormalInterfaceItem {
    def apply(name: Name, parent: ParentItem, tparams: List[TypeVar] = Nil,
              interfaces: List[ClassType] = Nil, fields: Set[String] = Set(),
              constructors: => Array[ConstructorItem] = noConstructors): ClassItem =
      new NormalInterfaceItem(name,parent,tparams,interfaces,fields,constructors)
  }

  class NormalClassItem(val name: Name, val parent: ParentItem, val tparams: List[TypeVar] = Nil,
                        val base: ClassType = ObjectType, val interfaces: List[ClassType] = Nil,
                        val isFinal: Boolean = false, val fields: Set[String] = Set(),
                        _constructors: => Array[ConstructorItem] = noConstructors) extends ClassItem {
    def supers = base :: interfaces
    def superItems = supers map (_.item)
    def isClass = true
    def isEnum = false
    def declaresField(kid: Name) = fields contains kid
    lazy val constructors = _constructors

    // Not sure why we need these
    def canEqual(x: Any) = x match { case x:AnyRef => this eq x; case _ => false }
    def productArity = notImplemented
    def productElement(n: Int) = notImplemented
  }
  object NormalClassItem {
    def apply(name: Name, parent: ParentItem, tparams: List[TypeVar] = Nil,
              base: ClassType = ObjectType, interfaces: List[ClassType] = Nil,
              isFinal: Boolean = false, fields: Set[String] = Set(),
              constructors: => Array[ConstructorItem] = noConstructors): ClassItem =
      new NormalClassItem(name,parent,tparams,base,interfaces,isFinal,fields,constructors)
  }

  case class UnresolvedClassItem(name: Name, pkgName: Name, args: List[TypeArg], isFinal: Boolean) extends ClassItem {
    def isClass = true
    def isEnum = false
    val parent = PackageItem(pkgName, pkgName)
    def base = ObjectType
    def supers = List(base)
    def superItems = List(ObjectItem)
    lazy val tparams = (1 to args.size).map(x => SimpleTypeVar("T"+x)).toList

    def generic: ClassType = generic(args, parent)

    def declaresField(kid: Name) = false
    val constructors = noConstructors
  }

  case object ArrayItem extends RefTypeItem {
    def name = "Array"
    override def qualifiedName = None
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
    override def qualifiedName = None
    def supers = Nil
    def superItems = Nil
    private def error = throw new RuntimeException("NoTypeItem shouldn't be touched")
    def inside = error
    def raw = error
    def simple = error
  }

  trait Member extends Item {
    def name: Name
    def parent: PlaceItem // Could be a package
    def qualifiedName = parent.qualifiedName map {
      case "" => name
      case s => s + "." + name
    }
  }
  trait ClassMember extends Member {
    def parent: ClassItem
  }

  // Values
  sealed abstract class Value extends Item {
    def item: TypeItem // The item of our type
    def isFinal: Boolean
  }
  sealed abstract class StaticValue extends Value {
    def ty: Type
    def item = ty.item
  }
  sealed abstract class LocalValue extends Value {
    def qualifiedName = None
    override def toString = "local:" + name
    def ty: Type
    def item = ty.item
  }
  case class ThisItem(self: ClassItem) extends Value with PseudoCallableItem {
    def name = "this"
    def qualifiedName = None
    def item = self
    def inside = self.inside
    def isFinal = true
  }
  case class SuperItem(self: ClassType) extends Value with PseudoCallableItem {
    def name = "super"
    def qualifiedName = None
    def item = self.item
    def inside = self
    def isFinal = true
  }
  abstract class FieldItem extends Value with ClassMember {
    def inside: Type
    def item = inside.item
  }
  abstract class StaticFieldItem extends StaticValue with ClassMember
  case class ParameterItem(name: Name, ty: Type, isFinal: Boolean) extends LocalValue
  case class LocalVariableItem(name: Name, ty: Type, isFinal: Boolean) extends LocalValue
  case class EnumConstantItem(name: Name, parent: ClassItem) extends StaticValue with ClassMember {
    assert(parent.isEnum) // TODO: make a separate maker for enums?
    override def item = parent
    def ty = parent.simple
    def isFinal = true
  }
  case class LitValue(x: Lit) extends StaticValue {
    val name = show(x)
    val qualifiedName = Some(name)
    val ty = x.ty
    override val item = x.item
    def isFinal = true
  }

  // Normal values
  case class NormalFieldItem(name: Name, inside: Type, parent: ClassItem, isFinal: Boolean) extends FieldItem
  case class NormalStaticFieldItem(name: Name, ty: Type, parent: ClassItem, isFinal: Boolean) extends StaticFieldItem

  // Callables
  sealed trait PseudoCallableItem extends Item // Callable or this or super
  sealed abstract class CallableItem extends PseudoCallableItem with PlaceItem with GenericItem {
    def params: List[Type]
  }
  sealed trait CallableParentItem extends CallableItem with SimpleParentItem {
    def parent: ClassItem
    def simple: Parent = throw new RuntimeException("For CallableParentItem, only inside is valid, not simple")
  }
  abstract class MethodItem extends CallableParentItem with ClassMember {
    def retVal: Type
    def isStatic: Boolean
  }
  abstract class ConstructorItem extends CallableParentItem with ClassMember {
    def name = parent.name
  }

  // Normal callables
  case class NormalMethodItem(name: Name, parent: ClassItem, tparams: List[TypeVar], retVal: Type,
                              params: List[Type], isStatic: Boolean) extends MethodItem
  case class NormalConstructorItem(parent: ClassItem, tparams: List[TypeVar],
                                   params: List[Type]) extends ConstructorItem
}
