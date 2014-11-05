package tarski

import tarski.AST._
import ambiguity.Utility.notImplemented

object Items {
  // represents a language item (these are given to us by someone who knows about the surrounding code)
  sealed abstract class EnvItem

  // anything with this type will never be looked up (mainly, errors)
  sealed trait NoLookupItem

  sealed abstract class NamedItem(val name: Name) extends EnvItem {
    // Need no-arg constructor for serialization
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

  // A method or constructor
  sealed abstract class CallableItem(name: Name, val paramTypes: List[Type]) extends NamedItem(name) with scala.Serializable

  // Miscellaneous
  case class PackageItem(override val name: Name, qualifiedName: Name, relativeName: Name) extends NamedItem(name) with scala.Serializable
  case class AnnotationItem(override val name: Name, qualifiedName: Name, relativeName: Name) extends NamedItem(name) with scala.Serializable

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
  sealed abstract class NumType(name: Name) extends PrimType(name)
  case object BooleanType extends PrimType("boolean")
  case object ByteType    extends NumType("byte")
  case object ShortType   extends NumType("short")
  case object IntType     extends NumType("int")
  case object LongType    extends NumType("long")
  case object FloatType   extends NumType("float")
  case object DoubleType  extends NumType("double")
  case object CharType    extends NumType("char")

  sealed abstract class RefType(name: Name) extends Type(name) with scala.Serializable
  case class ErrorType(override val name: Name) extends RefType(name) with scala.Serializable {
    def qualifiedName = name
    def relativeName = name
    def base = ObjectType
  }
  case class InterfaceType(override val name: Name, containing: NamedItem, relativeName: Name, base: RefType = ObjectType)
    extends RefType(name) with Member with scala.Serializable
  sealed abstract class ClassOrObjectType(override val name: Name, val containing: NamedItem, val relativeName: Name)
    extends RefType(name) with Member with scala.Serializable
  case class ClassType(override val name: Name, override val containing: NamedItem, override val relativeName: Name,
                       base: RefType, implements: List[InterfaceType] = Nil)
    extends ClassOrObjectType(name,containing,relativeName) with Member with scala.Serializable
  object ObjectType extends ClassOrObjectType("Object",JavaLangPkg,"Object")
  case class EnumType(override val name: Name, containing: NamedItem, override val relativeName: Name)
    extends RefType(name) with Member with scala.Serializable {
    def base = EnumBaseType
  }
  case class IntersectType(ts: Set[RefType]) extends RefType(s"IntersectType(${ts.map(_.toString).mkString(",")})") {
    def qualifiedName = this.name // TODO: These make little sense
    def relativeName = this.name
  }

  trait Member {
    def name: Name
    def containing: NamedItem

    def qualifiedName: Name = if (containing.qualifiedName.isEmpty) name else containing.qualifiedName + '.' + name
  }

  trait ClassMember extends Member {
    def name: Name
    def containing: RefType
  }


  // null is special
  case object NullType extends RefType("nulltype") with scala.Serializable {
    override def qualifiedName = "nulltype"
    def relativeName = "nulltype"
  }

  val JavaLangPkg = PackageItem("java.lang", "java.lang", "java.lang")
  val JavaIoPkg = PackageItem("java.io", "java.io", "java.io")
  val LocalPkg = PackageItem("", "", "")

  // Common references types are important enough to name
  private def commonRef(name: String, base: ClassOrObjectType, implements: List[InterfaceType] = Nil) =
    ClassType(name,JavaLangPkg,name,base,implements)

  // we need some basic interfaces
  // TODO: this is ugly, the relative names must change (maybe make that a function?)
  val Serializable = InterfaceType("Serializable", JavaIoPkg, "java.io.Serializable")
  val CharSequence = InterfaceType("CharSequence", JavaLangPkg, "CharSequence")
  // TODO: without proper generics support, this is what Comparable<T> looks like for now
  private def comparableRef(name: String) = InterfaceType("Comparable<"+name+">", JavaLangPkg, "Comparable<"+name+">")
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
  
  val NumberType = ClassType("Number", JavaLangPkg, "Number", ObjectType, List(Serializable))

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
    def qualifiedName = inner.qualifiedName + "[]"
    def relativeName = inner.relativeName + "[]"
  }

  // Values
  case class FieldItem(override val name: Name, override val ourType: Type, containing: ClassType, relativeName: Name)
    extends Value(name, ourType) with ClassMember with scala.Serializable
  case class StaticFieldItem(override val name: Name, override val ourType: Type, containing: ClassType, relativeName: Name)
    extends Value(name, ourType) with ClassMember with scala.Serializable
  case class ParameterItem(override val name: Name, override val ourType: Type)
    extends Value(name, ourType) with LocalItem with scala.Serializable
  case class LocalVariableItem(override val name: Name, override val ourType: Type)
    extends Value(name, ourType) with LocalItem with scala.Serializable
  case class EnumConstantItem(override val name: Name, override val ourType: EnumType) extends Value(name, ourType) with ClassMember with scala.Serializable {
    def containing = ourType
    def relativeName = ourType.relativeName + '.' + name // TODO: not correct if we're in the same match
  }

  // callables
  case class MethodItem(override val name: Name, override val containing: RefType, relativeName: Name,
                        retVal: Type, override val paramTypes: List[Type])
    extends CallableItem(name, paramTypes) with ClassMember with scala.Serializable
  case class StaticMethodItem(override val name: Name, override val containing: RefType, relativeName: Name,
                              retVal: Type, override val paramTypes: List[Type])
    extends CallableItem(name, paramTypes) with ClassMember with scala.Serializable


  sealed class ConstructorItem(val containing: ClassType, paramTypes: List[Type]) extends CallableItem(containing.name, paramTypes) with ClassMember with scala.Serializable {
    def relativeName = containing.relativeName + "." + name // TODO: Not correct if we're in the same match
   }

  sealed class TypeParameterType(name: String) extends Type(name) with NoLookupItem with scala.Serializable {
    def qualifiedName = "notImplemented"
    def relativeName = "notImplemented"
  }

  // items that have no qualified names
  sealed trait LocalItem {
    def name: String

    def qualifiedName = "" // TODO: this should probably be None and does require special treatment
    def relativeName = name // we may still be shadowed, but local items are only part of the environment if they are visible
    override def toString = "local:" + name
  }
}
