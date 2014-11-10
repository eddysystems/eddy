package tarski

import tarski.AST._
import tarski.Types._
import tarski.Base.{JavaLangPkg,EnumBaseItem}

object Items {
  // A language item, given to us by someone who knows about the surrounding code
  sealed abstract class Item

  // Anything with this type will never be looked up (mainly, errors)
  sealed trait NoLookupItem

  sealed abstract class NamedItem(val name: Name) extends Item {
    // Need no-arg constructor for serialization
    def this() = { this("") }

    // used for name matching
    def qualifiedName: Name // TODO: need to be Option[String] for objects without them

    override def toString: String = qualifiedName
  }

  // Type parameters
  case class TypeParamItem(override val name: String)
    extends NamedItem(name) with NoLookupItem with scala.Serializable {
    def qualifiedName = name
  }

  // NamedItems that have a type
  sealed abstract class Value(name: Name) extends NamedItem(name) with scala.Serializable {
    def ourType: Type
  }

  // A method or constructor
  sealed abstract class CallableItem(name: Name, val tparams: List[TypeParamItem], val params: List[Type])
    extends NamedItem(name) with scala.Serializable

  // Miscellaneous
  case class PackageItem(override val name: Name, qualifiedName: Name) extends NamedItem(name) with scala.Serializable
  case class AnnotationItem(override val name: Name, qualifiedName: Name) extends NamedItem(name) with scala.Serializable

  // Classes and interfaces
  sealed abstract class TypeItem(name: Name) extends NamedItem(name) with Member with scala.Serializable {
    def params: List[TypeParamItem]
    def arity: Int = params.size
  }
  sealed abstract class RefTypeItem(name: Name) extends TypeItem(name) with scala.Serializable
  case class InterfaceItem(override val name: Name, container: NamedItem, params: List[TypeParamItem] = Nil,
                           bases: List[InterfaceType] = Nil) extends RefTypeItem(name)
  sealed abstract class ClassOrObjectItem(override val name: Name) extends RefTypeItem(name)
  case object ObjectItem extends ClassOrObjectItem("Object") {
    def container = JavaLangPkg
    def params = Nil
  }
  abstract class ClassItem(override val name: Name) extends ClassOrObjectItem(name) { // Inherited from in Base, so not sealed
    def container: NamedItem
    def params: List[TypeParamItem]
    def base: ClassOrObjectType
    def implements: List[InterfaceType]
  }
  case class NormalClassItem(override val name: Name, container: NamedItem, params: List[TypeParamItem],
                             base: ClassOrObjectType = ObjectType, implements: List[InterfaceType] = Nil)
    extends ClassItem(name)
  case class EnumItem(override val name: Name, container: NamedItem, implements: List[InterfaceType])
    extends ClassItem(name) {
    def params = Nil
    def base = GenericClassType(EnumBaseItem,List(SimpleClassType(this)))
  }

  trait Member {
    def name: Name
    def container: NamedItem // Could be a package
    def qualifiedName: Name = if (container.qualifiedName.isEmpty) name else container.qualifiedName + '.' + name
  }

  trait ClassMember extends Member {
    def name: Name
    def container: TypeItem
  }

  // Values
  case class ThisItem(self: ClassItem) extends Value("this") with scala.Serializable {
    def qualifiedName = self.qualifiedName + ".this"
    def ourType = {
      assert(self.params.isEmpty)
      SimpleClassType(self)
    }
  }
  case class FieldItem(override val name: Name, ourType: Type, container: ClassItem)
    extends Value(name) with ClassMember with scala.Serializable
  case class StaticFieldItem(override val name: Name, ourType: Type, container: TypeItem)
    extends Value(name) with ClassMember with scala.Serializable
  case class ParameterItem(override val name: Name, ourType: Type)
    extends Value(name) with LocalItem with scala.Serializable
  case class LocalVariableItem(override val name: Name, ourType: Type)
    extends Value(name) with LocalItem with scala.Serializable
  case class EnumConstantItem(override val name: Name, container: EnumItem)
    extends Value(name) with ClassMember with scala.Serializable {
    def ourType = SimpleClassType(container)
  }

  // Callables
  case class MethodItem(override val name: Name, override val container: TypeItem,
                        override val tparams: List[TypeParamItem], retVal: Type, override val params: List[Type])
    extends CallableItem(name,tparams,params) with ClassMember with scala.Serializable
  case class StaticMethodItem(override val name: Name, override val container: TypeItem,
                              override val tparams: List[TypeParamItem], retVal: Type, override val params: List[Type])
    extends CallableItem(name,tparams,params) with ClassMember with scala.Serializable
  case class ConstructorItem(container: ClassOrObjectItem,
                             override val tparams: List[TypeParamItem], override val params: List[Type])
    extends CallableItem(container.name,tparams,params) with ClassMember with scala.Serializable

  // Items that have no qualified names
  sealed trait LocalItem {
    def name: String
    def qualifiedName = "" // TODO: this should probably be None and does require special treatment
    override def toString = "local:" + name
  }
}
