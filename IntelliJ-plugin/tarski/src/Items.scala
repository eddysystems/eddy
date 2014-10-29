package tarski

import sun.jvm.hotspot.jdi.InterfaceTypeImpl
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
  sealed abstract class Type(name: Name) extends NamedItem(name)

  // NamedItems that have a type
  sealed abstract class Value(name: Name, val ourType: Type) extends NamedItem(name)

  // a method or constructor
  sealed abstract class Callable(name: Name, val paramTypes: List[Type]) extends NamedItem(name)

  // stuff
  sealed abstract class PackageItem(name: Name) extends NamedItem(name)
  sealed abstract class AnnotationItem(name: Name) extends NamedItem(name)

  // Void is not properly a type in Java-land, but it is for us
  case object VoidType extends Type("void") {
    def qualifiedName = "void"
    def relativeName = "void"
  }

  // Primitive types (they'll also be in the environment, but they're nice to have around)
  sealed abstract class PrimType(name: Name) extends Type(name) {
    def qualifiedName = name
    def relativeName = name
  }
  case object BooleanType extends PrimType("boolean")
  case object ByteType    extends PrimType("byte")
  case object ShortType   extends PrimType("short")
  case object IntType     extends PrimType("int")
  case object LongType    extends PrimType("long")
  case object FloatType   extends PrimType("float")
  case object DoubleType  extends PrimType("double")
  case object CharType    extends PrimType("char")

  sealed abstract class RefType(name: Name) extends Type(name)
  sealed abstract class InterfaceType(name: Name, val base: InterfaceType = null) extends RefType(name)
  sealed abstract class ClassType(name: Name, val base: ClassType, val implements: List[InterfaceType] = Nil) extends RefType(name)
  sealed abstract class EnumType(name: Name) extends ClassType(name, EnumBaseType)

  // null is special
  case object NullType extends RefType("nulltype") {
    def qualifiedName = "nulltype"
    def relativeName = "nulltype"
  }

  // Common references types are important enough to name
  private def commonRef(name: String, base: ClassType, implements: List[InterfaceType]) = new ClassTypeImpl(name,"java.lang."+name,name,base,implements)

  val ObjectType = commonRef("Object", null, List())

  // we need some basic interfaces
  // TODO: this is ugly, the relative names must change (maybe make that a function?)
  val Serializable = new InterfaceTypeImpl("Serializable", "java.io.Serializable", "java.io.Serializable", null)
  val CharSequence = new InterfaceTypeImpl("CharSequence", "java.lang.CharSequence", "CharSequence", null)
  // TODO: without proper generics support, this is what Comparable<T> looks like for now
  private def comparableRef(name: String) = new InterfaceTypeImpl("Comparable<"+name+">", "java.lang.Comparable" + name + ">", "Comparable<"+name+">", null)
  val ComparableEnum = comparableRef("Enum")
  val ComparableString = comparableRef("String")
  val ComparableBoolean = comparableRef("Boolean")
  val ComparableCharacter = comparableRef("Character")
  val ComparableByte = comparableRef("Byte")
  val ComparableShort = comparableRef("Short")
  val ComparableInteger = comparableRef("Integer")
  val ComparableLong = comparableRef("Long")
  val ComparableFloat = comparableRef("Float")
  val ComparableDouble = comparableRef("Double")
  
  val NumberType = new ClassTypeImpl("Number", "java.lang.Number", "Number", ObjectType, List(Serializable))

  val EnumBaseType   = commonRef("Enum", ObjectType, List(Serializable, ComparableEnum))
  val StringType     = commonRef("String", ObjectType, List(Serializable, CharSequence, ComparableString))
  val BooleanRefType = commonRef("Boolean", ObjectType, List(Serializable, ComparableBoolean))
  val CharRefType    = commonRef("Character", ObjectType, List(Serializable, ComparableCharacter))
  val ByteRefType    = commonRef("Byte", NumberType, List(ComparableByte))
  val ShortRefType   = commonRef("Short", NumberType, List(ComparableShort))
  val IntRefType     = commonRef("Integer", NumberType, List(ComparableInteger))
  val LongRefType    = commonRef("Long", NumberType, List(ComparableLong))
  val FloatRefType   = commonRef("Float", NumberType, List(ComparableFloat))
  val DoubleRefType  = commonRef("Double", NumberType, List(ComparableDouble))

  case class ArrayType(inner: Type) extends RefType(inner.name + "[]") {
    override def qualifiedName = inner.qualifiedName + "[]"
    override def relativeName = inner.relativeName + "[]"
  }

  // values
  sealed abstract class FieldItem(name: Name, ourType: Type, val cls: ClassType) extends Value(name, ourType)
  sealed abstract class ParameterItem(name: Name, ourType: Type) extends Value(name, ourType)
  sealed abstract class LocalVariableItem(name: Name, ourType: Type) extends Value(name, ourType)
  sealed class EnumConstantItem(name: Name, val ourType: EnumType) extends NamedItem(name) {
    def qualifiedName = ourType.qualifiedName + '.' + name
    def relativeName = ourType.relativeName + '.' + name
  }

  // callables
  sealed abstract class MethodItem(name: Name, val retVal: Type, paramTypes: List[Type]) extends Callable(name, paramTypes)
  sealed class ConstructorItem(val cls: ClassType, paramTypes: List[Type]) extends Callable(cls.name, paramTypes) {
    def qualifiedName = cls.qualifiedName + "." + name
    def relativeName = cls.qualifiedName + "." + name // TODO: Not correct if we're in the same match
   }

  // things that are created by us
  // TODO: probably need all we have above here
  sealed abstract class NewVariableItem(name: Name, t: Type) extends NamedItem(name)
  sealed abstract class NewTypeItem(name: Name) extends Type(name)
  sealed abstract class NewMethodItem(name: Name) extends NamedItem(name)

  // when we cannot assign anything useful to this node
  sealed class ErrorItem() extends EnvItem
  sealed class ErrorType() extends Type("bad type") with NoLookupItem {
    def qualifiedName = "bad type"
    def relativeName = "bad type"
  }

  // These class implementations are created from the plugin side. They implement the matching interface defined in NamedItem
  class PackageItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends PackageItem(name)

  class ClassTypeImpl(name: Name, val qualifiedName: Name, val relativeName: Name, base: ClassType, implements: List[InterfaceType]) extends ClassType(name, base, implements)
  class InterfaceTypeImpl(name: Name, val qualifiedName: Name, val relativeName: Name, base: InterfaceType) extends InterfaceType(name, base)
  class EnumTypeImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends EnumType(name)

  class MethodItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name, retVal: Type, paramTypes: List[Type]) extends MethodItem(name, retVal, paramTypes)

  // items that have no qualified names
  sealed trait LocalItem {
    def name: String

    def qualifiedName = null
    def relativeName = null
    override def toString = "local:" + name
  }

  class ParameterItemImpl(name: Name, ourType: Type) extends ParameterItem(name, ourType) with LocalItem
  class LocalVariableItemImpl(name: Name, ourType: Type) extends LocalVariableItem(name, ourType) with LocalItem
  class FieldItemImpl(name: Name, ourType: Type, cls: ClassType, val qualifiedName: Name, val relativeName: Name) extends FieldItem(name, ourType, cls)
}