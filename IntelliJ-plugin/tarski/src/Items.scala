package tarski

import ambiguity.Utility.RefEq
import tarski.AST._
import tarski.Types._
import tarski.Base.{JavaLangPkg,EnumBaseItem,SerializableItem,CloneableItem}
import tarski.Pretty.pretty
import tarski.Tokens.show

object Items {
  // A language item, given to us by someone who knows about the surrounding code
  sealed abstract class Item extends scala.Serializable {
    def name: Name
    def qualifiedName: Option[Name] // A name that is valid anywhere
    override def toString: String = qualifiedName getOrElse name
  }

  // Anything with this type will never be looked up (mainly, errors)
  sealed trait NoLookupItem extends Item

  // Something which we can be inside
  sealed trait PlaceItem extends Item
  sealed trait ParentItem extends PlaceItem {
    def inside: Parent
    def raw: Parent
    def simple: Parent
  }

  // Type parameters.  Must be abstract for lazy generation of fresh variables (which can be recursive).
  abstract class TypeVar extends TypeItem with NoLookupItem with RefEq {
    def name: Name
    def lo: RefType // Lower bound
    def hi: RefType // Upper bound

    def qualifiedName = None
    def supers = List(lo)
    def simple = ParamType(this)
    def inside = simple
    def raw = simple

    def known(implicit env: Tenv): Boolean = env.get(this) match {
      case Some(None) => false // We're raw, and therefore not known
      case _ => true
    }
  }
  case class NormalTypeVar(name: String, base: RefType, implements: List[ClassType]) extends TypeVar {
    override def supers = base :: implements
    def lo = NullType
    def hi = glb(supers)
  }
  case class SimpleTypeVar(name: String) extends TypeVar {
    def lo = NullType
    def hi = ObjectType
  }

  // Packages
  case class PackageItem(name: Name, qualified: Name) extends Item with ParentItem with PackageParent {
    def item = this
    def qualifiedName = Some(qualified)
    def inside = this
    def simple = this
  }

  // Annotations
  case class AnnotationItem(name: Name, qualified: Name) extends Item {
    def qualifiedName = Some(qualified)
  }

  // Types
  sealed abstract class TypeItem extends Item {
    def supers: List[RefType]
    def inside: Type
    def raw: Type
    def simple: Type
  }
  case class LangTypeItem(t: LangType) extends TypeItem {
    def name = show(pretty(t))
    def qualifiedName = Some(name)
    def supers = Nil
    def inside = t
    def raw = t
    def simple = t
  }
  sealed abstract class RefTypeItem extends TypeItem with Member {
    def params: List[TypeVar]
    def arity: Int = params.size
    def raw: ClassType
    def generic(args: List[TypeArg], parent: Parent): ClassType
  }

  abstract class ClassItem extends RefTypeItem with ParentItem {
    def parent: ParentItem
    def isClass: Boolean // true for class, false for interface
    def isEnum: Boolean // true only for descendants of Enum<E>
    def base: ClassType
    def implements: List[ClassType]
    def supers = base :: implements

    // Can we unbox to a primitive type?
    def unbox: Option[PrimType] = None
    def unboxNumeric: Option[NumType] = None
    def unboxIntegral: Option[IntegralType] = None
    def unboxesToNumeric: Boolean = false
    def unboxesToBoolean: Boolean = false

    // Convert to the type valid inside the definition
    val inside: ClassType = {
      val p = parent.inside
      if (arity == 0) SimpleType(this,p)
      else GenericType(this,params map ParamType,p)
    }

    // Convert to a type valid anywhere, bailing if type parameters are required
    def simple: SimpleType =
      if (arity == 0) SimpleType(this,parent.inside)
      else throw new RuntimeException(s"class $name isn't simple (has args $params)")

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
  }

  class ClassItemMaker(val name: Name, val parent: ParentItem, var params: List[TypeVar],
                       val isClass: Boolean, val isEnum: Boolean) extends ClassItem {
    // To be filled in later
    private var _base: ClassType = null
    private var _implements: List[ClassType] = null

    // Public interface
    def base = { assert(_base ne null); _base }
    def implements = { assert(_base ne null); _implements }

    def set(base: ClassType, implements: List[ClassType]): Unit = {
      assert(_base eq null)
      _base = base
      _implements = implements
    }
  }

  class TypeVarMaker(val name: Name) extends TypeVar {
    // To be filled in later
    private var _base: ClassType = null
    private var _implements: List[ClassType] = null

    // Public interface
    override def supers = { assert(_base != null); _base :: _implements }
    def lo = NullType
    def hi = glb(supers)

    def set(base: ClassType, implements: List[ClassType]): Unit = {
      assert(_base eq null)
      _base = base
      _implements = implements
    }
  }

  case object ObjectItem extends ClassItem {
    def name = "Object"
    def parent = JavaLangPkg
    def isClass = true
    def isEnum = false
    def params = Nil
    override def base = throw new RuntimeException("Object has no base")
    def implements = Nil
    override def supers = Nil
    override val inside = ObjectType
    override def raw = ObjectType
    override def generic(args: List[TypeArg], par: Parent) = {
      if (par.item != parent) throw new RuntimeException(s"parent mismatch: expected $parent, got $par}")
      if (args.nonEmpty) throw new RuntimeException("Object takes no arguments")
      ObjectType
    }
  }

  case class NormalInterfaceItem(name: Name, parent: ParentItem, params: List[TypeVar] = Nil,
                                 implements: List[ClassType] = Nil) extends ClassItem {
    def base = ObjectType
    def isClass = false
    def isEnum = false
  }

  case class NormalClassItem(name: Name, parent: ParentItem, params: List[TypeVar],
                             base: ClassType = ObjectType, implements: List[ClassType] = Nil) extends ClassItem {
    def isClass = true
    def isEnum = false
  }

  case class EnumItem(name: Name, parent: ParentItem, implements: List[ClassType]) extends ClassItem {
    def isClass = true
    def isEnum = true
    def params = Nil
    def base = GenericType(EnumBaseItem,List(inside),JavaLangPkg)
  }

  case object ArrayItem extends RefTypeItem with NoLookupItem {
    def name = "Array"
    override def qualifiedName = None
    def parent = JavaLangPkg
    private def error = throw new RuntimeException("Array<T> is special: T can be primitive, and is covariant")
    def params = error
    val supers = List(SerializableItem.simple,CloneableItem.simple)
    def inside = error
    def raw = error
    def simple = error
    def generic(args: List[TypeArg], parent: Parent) = error
  }
  case object NoTypeItem extends TypeItem {
    def name = "NoTypeItem"
    def qualifiedName = None
    def supers = Nil
    private def error = throw new RuntimeException("NoTypeItem shouldn't be touched")
    def inside = error
    def raw = error
    def simple = error
  }

  trait Member {
    def name: Name
    def parent: PlaceItem // Could be a package
    def qualifiedName = parent.qualifiedName map (_ + '.' + name)
  }

  trait ClassMember extends Member {
    def name: Name
    def parent: TypeItem with ParentItem
  }

  // Values
  sealed abstract class Value extends Item {
    def item: TypeItem // The item of our type
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
  case class ThisItem(self: ClassItem) extends Value {
    def name = "this"
    def qualifiedName = None
    def item = self
    def inside = self.inside
  }
  case class FieldItem(name: Name, inside: Type, parent: ClassItem) extends Value with ClassMember {
    def item = inside.item
  }
  case class StaticFieldItem(name: Name, ty: Type, parent: ClassItem) extends StaticValue with ClassMember
  case class ParameterItem(name: Name, ty: Type) extends LocalValue
  case class LocalVariableItem(name: Name, ty: Type) extends LocalValue
  case class EnumConstantItem(name: Name, parent: EnumItem) extends StaticValue with ClassMember {
    override def item = parent
    def ty = parent.simple
  }

  // Callables
  sealed abstract class CallableItem extends Item with PlaceItem {
    def tparams: List[TypeVar]
    def params: List[Type]
  }
  case class MethodItem(name: Name, parent: ClassItem, tparams: List[TypeVar], retVal: Type,
                        params: List[Type]) extends CallableItem with ClassMember
  case class StaticMethodItem(name: Name, parent: ClassItem, tparams: List[TypeVar], retVal: Type,
                              params: List[Type]) extends CallableItem with ClassMember
  case class ConstructorItem(parent: ClassItem, tparams: List[TypeVar], params: List[Type])
    extends CallableItem with ClassMember {
    def name = parent.name
  }
}
