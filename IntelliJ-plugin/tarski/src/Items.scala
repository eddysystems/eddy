package tarski

import tarski.AST._

object Items {
  // represents a language item (these are given to us by someone who knows about the surrounding code)
  sealed abstract class EnvItem

  // anything with this type will never be looked up (mainly, errors)
  sealed trait NoLookupItem

  sealed abstract class NamedItem(val name: Name) extends EnvItem {
    // used for name matching
    def qualifiedName: Name
    def relativeName: Name

    override def toString: String = qualifiedName
  }

  sealed trait ContainedItem {
    def container: NamedItem
  }

  // EnvItems that are a type-like thing
  sealed abstract class TypeItem(name: Name) extends NamedItem(name)

  // NamedItems that have a type
  sealed abstract class ValueItem(name: Name, val ourType: TypeItem) extends NamedItem(name)

  // a method or constructor
  sealed abstract class CallableItem(name: Name, val paramTypes: List[TypeItem]) extends NamedItem(name)

  // stuff
  sealed abstract class PackageItem(name: Name) extends NamedItem(name)
  sealed abstract class AnnotationItem(name: Name) extends NamedItem(name)

  // types
  sealed abstract class BasicTypeItem(name: Name) extends TypeItem(name) {
    def qualifiedName = name
    def relativeName = name
  }

  // Java basic types (they'll also be in the environment, but they're nice to have around)
  object BooleanItem extends BasicTypeItem("boolean")
  object IntItem extends BasicTypeItem("int")
  object FloatItem extends BasicTypeItem("float")
  object LongItem extends BasicTypeItem("long")
  object DoubleItem extends BasicTypeItem("double")
  object CharItem extends BasicTypeItem("char")
  object ShortItem extends BasicTypeItem("short")
  object VoidItem extends BasicTypeItem("void")

  object NullTypeItem extends BasicTypeItem("nulltype")

  sealed abstract class ClassItem(name: Name) extends TypeItem(name)
  sealed abstract class EnumItem(name: Name) extends ClassItem(name)
  sealed abstract class InterfaceItem(name: Name) extends ClassItem(name)

  class ArrayTypeItem(val inner: TypeItem, val dims: Int) extends TypeItem(inner.name + "[]") {
    override def qualifiedName = inner.qualifiedName + "[]"
    override def relativeName = inner.relativeName + "[]"
  }

  // values
  sealed abstract class FieldItem(name: Name, ourType: TypeItem, val cls: ClassItem) extends ValueItem(name, ourType)
  sealed abstract class ParameterItem(name: Name, ourType: TypeItem) extends ValueItem(name, ourType)
  sealed abstract class LocalVariableItem(name: Name, ourType: TypeItem) extends ValueItem(name, ourType)
  sealed class EnumConstantItem(name: Name, val ourType: EnumItem) extends NamedItem(name) {
    def qualifiedName = ourType.qualifiedName + '.' + name
    def relativeName = ourType.relativeName + '.' + name
  }

  // callables
  sealed abstract class MethodItem(name: Name, val retVal: TypeItem, paramTypes: List[TypeItem]) extends CallableItem(name, paramTypes)
  sealed class ConstructorItem(val cls: ClassItem, paramTypes: List[TypeItem]) extends CallableItem(cls.name, paramTypes) {
    def qualifiedName = cls.qualifiedName + "." + name
    def relativeName = cls.qualifiedName + "." + name // TODO: Not correct if we're in the same match
   }

  // things that are created by us
  // TODO: probably need all we have above here
  sealed abstract class NewVariableItem(name: Name, t: TypeItem) extends NamedItem(name)
  sealed abstract class NewTypeItem(name: Name) extends TypeItem(name)
  sealed abstract class NewMethodItem(name: Name) extends NamedItem(name)

  // when we cannot assign anything useful to this node
  sealed class ErrorItem() extends EnvItem
  sealed class ErrorTypeItem() extends TypeItem("bad type") with NoLookupItem {
    def qualifiedName = "bad type"
    def relativeName = "bad type"
  }

  // These class implementations are created from the plugin side. They implement the matching interface defined in NamedItem
  class PackageItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends PackageItem(name)

  class ClassItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends ClassItem(name)
  class InterfaceItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends InterfaceItem(name)
  class EnumItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends EnumItem(name)

  class MethodItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name, retVal: TypeItem, paramTypes: List[TypeItem]) extends MethodItem(name, retVal, paramTypes)

  // items that have no qualified names
  sealed trait LocalItem {
    def name: String

    def qualifiedName = null
    def relativeName = null
    override def toString = "local:" + name
  }

  class ParameterItemImpl(name: Name, ourType: TypeItem) extends ParameterItem(name, ourType) with LocalItem
  class LocalVariableItemImpl(name: Name, ourType: TypeItem) extends LocalVariableItem(name, ourType) with LocalItem
  class FieldItemImpl(name: Name, ourType: TypeItem, cls: ClassItem, val qualifiedName: Name, val relativeName: Name) extends FieldItem(name, ourType, cls)
}
