package tarski

import tarski.AST._
import ambiguity.Utility.notImplemented

object Items {
  // represents a language item (these are given to us by someone who knows about the surrounding code)
  sealed abstract class EnvItem

  // anything with this type will never be looked up (mainly, errors)
  sealed trait NoLookupItem

  sealed abstract class NamedItem(val name: Name) extends EnvItem {

    // need no-arg constructor for serialization
    def this() = { this("") }

    // used for name matching
    def qualifiedName: Name // TODO: need to be Option[String] for objects without them
    def relativeName: Name

    override def toString: String = qualifiedName
  }

  // EnvItems that are a type-like thing
  sealed abstract class Type(name: Name) extends NamedItem(name) with scala.Serializable

  // NamedItems that have a type
  sealed abstract class Value(name: Name, val ourType: Type) extends NamedItem(name) with scala.Serializable

  // a method or constructor
  sealed abstract class Callable(name: Name, val paramTypes: List[Type]) extends NamedItem(name) with scala.Serializable

  // stuff
  sealed abstract class PackageItem(name: Name) extends NamedItem(name) with scala.Serializable
  sealed abstract class AnnotationItem(name: Name) extends NamedItem(name) with scala.Serializable

  // Void is not properly a type in Java-land, but it is for us
  case object VoidType extends Type("void") with scala.Serializable {
    def qualifiedName = "void"
    def relativeName = "void"
  }

  // Primitive types (they'll also be in the environment, but they're nice to have around)
  sealed abstract class PrimType(name: Name) extends Type(name) with scala.Serializable {
    def qualifiedName = name
    def relativeName = name
  }
  case object BooleanType extends PrimType("boolean") with scala.Serializable
  case object ByteType    extends PrimType("byte")
  case object ShortType   extends PrimType("short")
  case object IntType     extends PrimType("int")
  case object LongType    extends PrimType("long")
  case object FloatType   extends PrimType("float")
  case object DoubleType  extends PrimType("double")
  case object CharType    extends PrimType("char")

  sealed abstract class RefType(name: Name) extends Type(name) with scala.Serializable { def base: RefType }
  sealed abstract class InterfaceType(name: Name, val base: InterfaceType = null) extends RefType(name) with Member with scala.Serializable
  sealed abstract class ClassType(name: Name, val base: RefType, val implements: List[InterfaceType] = Nil) extends RefType(name) with Member with scala.Serializable
  sealed abstract class EnumType(name: Name) extends ClassType(name, EnumBaseType) with Member with scala.Serializable

  trait Member {
    def name: String
    def containing: NamedItem

    def qualifiedName: String = containing.qualifiedName + '.' + name
  }

  trait ClassMember extends Member {
    def name: String
    def containing: RefType
  }


  // null is special
  case object NullType extends RefType("nulltype") with scala.Serializable {
    def base = null
    override def qualifiedName = "nulltype"
    def relativeName = "nulltype"
  }

  val JavaLangPkg = new PackageItemImpl("java.lang", "java.lang", "java.lang")
  val JavaIoPkg = new PackageItemImpl("java.io", "java.io", "java.io")

  // Common references types are important enough to name
  private def commonRef(name: String, base: ClassType, implements: List[InterfaceType]) = new ClassTypeImpl(name,JavaLangPkg,name,base,implements)

  val ObjectType = commonRef("Object", null, List())

  // we need some basic interfaces
  // TODO: this is ugly, the relative names must change (maybe make that a function?)
  val Serializable = new InterfaceTypeImpl("Serializable", JavaIoPkg, "java.io.Serializable", null)
  val CharSequence = new InterfaceTypeImpl("CharSequence", JavaLangPkg, "CharSequence", null)
  // TODO: without proper generics support, this is what Comparable<T> looks like for now
  private def comparableRef(name: String) = new InterfaceTypeImpl("Comparable<"+name+">", JavaLangPkg, "Comparable<"+name+">", null)
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
  
  val NumberType = new ClassTypeImpl("Number", JavaLangPkg, "Number", ObjectType, List(Serializable))

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

  case class ArrayType(inner: Type) extends RefType(inner.name + "[]") with scala.Serializable {
    def base = ObjectType
    override def qualifiedName = inner.qualifiedName + "[]"
    override def relativeName = inner.relativeName + "[]"
  }

  // values
  sealed abstract class FieldItem(name: Name, ourType: Type) extends Value(name, ourType) with scala.Serializable
  sealed abstract class StaticFieldItem(name: Name, ourType: Type) extends Value(name, ourType) with scala.Serializable
  sealed abstract class ParameterItem(name: Name, ourType: Type) extends Value(name, ourType) with scala.Serializable
  sealed abstract class LocalVariableItem(name: Name, ourType: Type) extends Value(name, ourType) with scala.Serializable
  sealed class EnumConstantItem(name: Name, override val ourType: EnumType) extends Value(name, ourType) with ClassMember with scala.Serializable {
    def containing = ourType
    def relativeName = ourType.relativeName + '.' + name // TODO: not correct if we're in the same match
  }

  // callables
  sealed abstract class MethodItem(name: Name, val retVal: Type, paramTypes: List[Type]) extends Callable(name, paramTypes) with scala.Serializable
  sealed abstract class StaticMethodItem(name: Name, val retVal: Type, paramTypes: List[Type]) extends Callable(name, paramTypes) with scala.Serializable
  sealed class ConstructorItem(val containing: ClassType, paramTypes: List[Type]) extends Callable(containing.name, paramTypes) with ClassMember with scala.Serializable {
    def relativeName = containing.relativeName + "." + name // TODO: Not correct if we're in the same match
   }

  // when we cannot assign anything useful to this node
  sealed class ErrorItem() extends EnvItem with scala.Serializable
  sealed class ErrorType() extends Type("bad type") with NoLookupItem with scala.Serializable {
    def qualifiedName = "bad type"
    def relativeName = "bad type"
  }
  sealed class TypeParameterType(name: String) extends Type(name) with NoLookupItem with scala.Serializable {
    def qualifiedName = "notImplemented"
    def relativeName = "notImplemented"
  }

  // These class implementations are created from the plugin side. They implement the matching interface defined in NamedItem
  class PackageItemImpl(name: Name, val qualifiedName: Name, val relativeName: Name) extends PackageItem(name) with scala.Serializable

  class ClassTypeImpl(name: Name, val containing: NamedItem, val relativeName: Name, base: RefType, implements: List[InterfaceType]) extends ClassType(name, base, implements) with scala.Serializable
  class InterfaceTypeImpl(name: Name, val containing: NamedItem, val relativeName: Name, base: InterfaceType) extends InterfaceType(name, base) with scala.Serializable
  class EnumTypeImpl(name: Name, val containing: NamedItem, val relativeName: Name) extends EnumType(name) with scala.Serializable

  class MethodItemImpl(name: Name, override val containing: RefType, val relativeName: Name, retVal: Type, paramTypes: List[Type]) extends MethodItem(name, retVal, paramTypes) with ClassMember with scala.Serializable
  class StaticMethodItemImpl(name: Name, override val containing: RefType, val relativeName: Name, retVal: Type, paramTypes: List[Type]) extends StaticMethodItem(name, retVal, paramTypes) with ClassMember with scala.Serializable

  // items that have no qualified names
  sealed trait LocalItem {
    def name: String

    def qualifiedName = null
    def relativeName = name // we may still be shadowed, but local items are only part of the environment if they are visible
    override def toString = "local:" + name
  }

  class ParameterItemImpl(name: Name, ourType: Type) extends ParameterItem(name, ourType) with LocalItem with scala.Serializable
  class LocalVariableItemImpl(name: Name, ourType: Type) extends LocalVariableItem(name, ourType) with LocalItem with scala.Serializable
  class FieldItemImpl(name: Name, ourType: Type, val containing: ClassType, val relativeName: Name) extends FieldItem(name, ourType) with ClassMember with scala.Serializable
  class StaticFieldItemImpl(name: Name, ourType: Type, val containing: ClassType, val relativeName: Name) extends StaticFieldItem(name, ourType) with ClassMember with scala.Serializable
}
