package tarski

import tarski.AST._
import tarski.Types._
import tarski.Base.{JavaLangPkg,EnumBaseItem,SerializableItem,CloneableItem}
import tarski.Pretty.pretty
import tarski.Tokens.show

object Items {
  // A language item, given to us by someone who knows about the surrounding code
  sealed abstract class Item

  sealed abstract class NamedItem extends Item with scala.Serializable {
    def name: Name

    // A name that is valid anywhere
    def qualifiedName: Option[Name]

    override def toString: String = qualifiedName getOrElse name
  }

  // Anything with this type will never be looked up (mainly, errors)
  sealed trait NoLookupItem extends NamedItem

  // Something which we can be inside
  sealed trait PlaceItem extends NamedItem
  sealed trait ParentItem extends PlaceItem {
    def inside: Parent
    def raw: Parent
    def simple: Parent
  }

  // Type parameters
  case class TypeParamItem(name: String, base: ClassType = ObjectType, implements: List[ClassType] = Nil)
    extends TypeItem with NoLookupItem {
    def qualifiedName = None
    def supers = base :: implements
    def simple = ParamType(this)
    def inside = simple
    def raw = simple
  }

  // NamedItems that have a type
  sealed abstract class Value extends NamedItem {
    def item: TypeItem // The item of our type
  }
  sealed abstract class StaticValue extends Value {
    def ty: Type
    def item = ty.item
  }

  // A method or constructor
  sealed abstract class CallableItem extends NamedItem with PlaceItem {
    def tparams: List[TypeParamItem]
    def params: List[Type]
  }

  // Miscellaneous
  case class PackageItem(name: Name, qualified: Name) extends NamedItem with ParentItem with Parent {
    def item = this
    def qualifiedName = Some(qualified)
    def inside = this
    def raw = this
    def simple = this
    def env = Map.empty
    def isRaw = false
    def isSimple = true
  }

  case class AnnotationItem(name: Name, qualified: Name) extends NamedItem {
    def qualifiedName = Some(qualified)
  }

  // Types
  sealed abstract class TypeItem extends NamedItem {
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
    def params: List[TypeParamItem]
    def arity: Int = params.size
    def raw: ClassType
    def generic(args: List[RefType], parent: Parent): ClassType
  }

  abstract class ClassItem extends RefTypeItem with ParentItem {
    def parent: ParentItem
    def isClass: Boolean // true for class, false for interface
    def base: ClassType
    def implements: List[ClassType]
    def supers = base :: implements

    // Convert to the type valid inside the definition
    val inside: ClassType = {
      val p = parent.inside
      if (arity == 0) SimpleClassType(this,p)
      else GenericClassType(this,params map ParamType,p)
    }

    // Convert to a type valid anywhere, bailing if type parameters are required
    def simple: SimpleClassType =
      if (arity == 0) SimpleClassType(this,parent.inside)
      else throw new RuntimeException("class isn't simple")

    // Convert to a simple or raw type (valid anywhere)
    def raw: ClassType = {
      def p = parent.raw
      if (arity == 0) SimpleClassType(this,p)
      else RawClassType(this,p)
    }

    // Convert to a type valid anywhere
    def generic(args: List[RefType], par: Parent): ClassType = {
      if (par.item != parent)
        throw new RuntimeException(s"parent mismatch: expected $parent, got $par}")
      if (arity != args.size)
        throw new RuntimeException(s"arity mismatch: $name takes $arity arguments, not ${args.size} ($args)")
      if (arity == 0) SimpleClassType(this,par)
      else GenericClassType(this,args,par)
    }
    def generic(args: List[RefType]): ClassType = generic(args,parent.simple)
  }

  case class NormalInterfaceItem(name: Name, parent: ParentItem, params: List[TypeParamItem] = Nil,
                                 implements: List[ClassType] = Nil) extends ClassItem {
    def base = ObjectType
    def isClass = false
  }
  case object ObjectItem extends ClassItem {
    def name = "Object"
    def parent = JavaLangPkg
    def isClass = true
    def params = Nil
    override def base = throw new RuntimeException("Object has no base")
    def implements = Nil
    override def supers = Nil
    override val inside = ObjectType
    override def raw = ObjectType
    override def generic(args: List[RefType], par: Parent) = {
      if (par.item != parent) throw new RuntimeException(s"parent mismatch: expected $parent, got $par}")
      if (!args.isEmpty) throw new RuntimeException("Object takes no arguments")
      ObjectType
    }
  }
  case class NormalClassItem(name: Name, parent: ParentItem, params: List[TypeParamItem],
                             base: ClassType = ObjectType, implements: List[ClassType] = Nil) extends ClassItem {
    def isClass = true
  }
  case class EnumItem(name: Name, parent: ParentItem, implements: List[ClassType]) extends ClassItem {
    def isClass = true
    def params = Nil
    def base = GenericClassType(EnumBaseItem,List(inside),JavaLangPkg)
  }
  case object ArrayItem extends RefTypeItem with NoLookupItem {
    def name = "Array"
    override def qualifiedName = None
    def parent = JavaLangPkg
    private def error = throw new RuntimeException("Array<T> is special: T can be primitive, and is covariant")
    def params = error
    val supers = List(SerializableType,CloneableType)
    def inside = error
    def raw = error
    def simple = error
    def generic(args: List[RefType], parent: Parent) = error
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
  case class ThisItem(self: ClassItem) extends Value {
    def name = "this"
    def qualifiedName = None
    def item = self
    def ty = self.inside
  }
  case class FieldItem(name: Name, ty: Type, parent: ClassItem) extends Value with ClassMember {
    def item = ty.item
  }
  case class StaticFieldItem(name: Name, ty: Type, parent: ClassItem) extends StaticValue with ClassMember
  case class ParameterItem(name: Name, ty: Type) extends Value with LocalItem {
    def item = ty.item
  }
  case class LocalVariableItem(name: Name, ty: Type) extends Value with LocalItem {
    def item = ty.item
  }
  case class EnumConstantItem(name: Name, parent: EnumItem) extends StaticValue with ClassMember {
    override def item = parent
    def ty = parent.simple
  }

  // Callables
  case class MethodItem(name: Name, parent: ClassItem, tparams: List[TypeParamItem], retVal: Type,
                        params: List[Type]) extends CallableItem with ClassMember
  case class StaticMethodItem(name: Name, parent: ClassItem, tparams: List[TypeParamItem], retVal: Type,
                              params: List[Type]) extends CallableItem with ClassMember
  case class ConstructorItem(parent: ClassItem, tparams: List[TypeParamItem], params: List[Type])
    extends CallableItem with ClassMember {
    def name = parent.name
  }

  // Items that have no qualified names
  sealed trait LocalItem {
    def name: String
    def qualifiedName = None
    override def toString = "local:" + name
  }
}
