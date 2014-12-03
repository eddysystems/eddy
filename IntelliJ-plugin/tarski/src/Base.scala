package tarski

import tarski.AST.Name
import tarski.Items._
import tarski.Types._
import tarski.Environment.Env

object Base {
  // Basic packages
  val JavaLangPkg = PackageItem("java.lang","java.lang")
  val JavaIoPkg = PackageItem("java.io","java.io")
  val LocalPkg = PackageItem("","")

  // Basic interfaces and classes
  val CloneableItem = NormalInterfaceItem("Cloneable",JavaLangPkg)
  val SerializableItem = NormalInterfaceItem("Serializable",JavaIoPkg)
  val CharSequenceItem = NormalInterfaceItem("CharSequence",JavaLangPkg)
  val ComparableItem = {
    val T = SimpleTypeVar("T")
    NormalInterfaceItem("Comparable",JavaLangPkg,List(T))
  }
  private def comparable(t: RefType): ClassType = GenericType(ComparableItem,List(t),JavaLangPkg)

  // Class Enum
  object EnumBaseItem extends ClassItem {
    def name = "Enum"
    def isClass = true
    def isEnum = true
    def isFinal = false
    private val E = SimpleTypeVar("E")
    def parent = JavaLangPkg
    def tparams = List(E)
    def base = ObjectType
    def implements = List(SerializableType,comparable(ParamType(E)))
  }

  // Simple classes
  sealed abstract class SimpleClassItem extends ClassItem {
    override def parent = JavaLangPkg
    override def tparams = Nil
    def isClass = true
    def isEnum = false
  }

  // Throwable
  object ThrowableItem extends SimpleClassItem {
    def name = "Throwable"
    def base = ObjectType
    def implements = Nil
    def isFinal = false
  }

  // Iterable
  object IterableItem extends ClassItem {
    def name = "Iterable"
    def isClass = false
    def isEnum = false
    def base = ObjectType
    private val T = SimpleTypeVar("T")
    def parent = JavaLangPkg
    def tparams = List(T)
    def implements = Nil
    def isFinal = false
  }

  // Class String
  object StringItem extends SimpleClassItem {
    def name = "String"
    def base = ObjectType
    def implements = List(comparable(inside),CharSequenceItem.simple,SerializableType)
    def isFinal = true
  }

  // java.lang.Void
  object VoidItem extends SimpleClassItem {
    def name = "Void"
    def base = ObjectType
    def implements = Nil
    def isFinal = true
  }

  // Reference wrappers around primitive types
  object BooleanItem extends SimpleClassItem {
    def name = "Boolean"
    def base = ObjectType
    def implements = List(comparable(inside),SerializableType)
    override def unbox = Some(BooleanType)
    override def unboxesToBoolean = true
    def isFinal = true
  }
  object CharacterItem extends SimpleClassItem {
    def name = "Character"
    def base = ObjectType
    def implements = List(comparable(inside),SerializableType)
    override def unbox = Some(CharType)
    override def unboxNumeric = Some(CharType)
    override def unboxIntegral = Some(CharType)
    def isFinal = true
  }
  object NumberItem extends SimpleClassItem {
    def name = "Number"
    def base = ObjectType
    def implements = List(SerializableType)
    def isFinal = false
  }
  sealed abstract class NumberClassItem(val name: Name, val ty: NumType) extends SimpleClassItem {
    def base = NumberItem.simple
    def implements = List(comparable(inside),SerializableType)
    override def unbox = Some(ty)
    override def unboxNumeric = Some(ty)
    def isFinal = true
  }
  sealed abstract class IntegralClassItem(name: Name, override val ty: IntegralType) extends NumberClassItem(name,ty) {
    override def unboxIntegral = Some(ty)
  }
  object ByteItem    extends NumberClassItem("Byte",ByteType)
  object ShortItem   extends NumberClassItem("Short",ShortType)
  object IntegerItem extends NumberClassItem("Integer",IntType)
  object LongItem    extends NumberClassItem("Long",LongType)
  object FloatItem   extends NumberClassItem("Float",FloatType)
  object DoubleItem  extends NumberClassItem("Double",DoubleType)

  object ubVoidItem    extends LangTypeItem(VoidType)
  object ubBooleanItem extends LangTypeItem(BooleanType)
  object ubByteItem    extends LangTypeItem(ByteType)
  object ubShortItem   extends LangTypeItem(ShortType)
  object ubIntItem     extends LangTypeItem(IntType)
  object ubLongItem    extends LangTypeItem(LongType)
  object ubFloatItem   extends LangTypeItem(FloatType)
  object ubDoubleItem  extends LangTypeItem(DoubleType)
  object ubCharItem    extends LangTypeItem(CharType)
  
  // Basic callables for test use
  val ObjectConsItem = ConstructorItem(ObjectItem,Nil,Nil)

  // Standard base environment
  val baseEnv = new Env(List(
    // Packages
    JavaLangPkg,JavaIoPkg,LocalPkg,
    // basic types
    ubVoidItem,ubBooleanItem,ubByteItem,ubShortItem,ubIntItem,ubLongItem,ubFloatItem,ubDoubleItem,ubCharItem,
    // Classes
    ObjectItem,ObjectConsItem,VoidItem,
    EnumBaseItem,ThrowableItem,StringItem,BooleanItem,CharacterItem,
    NumberItem,ByteItem,ShortItem,IntegerItem,LongItem,FloatItem,DoubleItem,
    // Interfaces
    CloneableItem,SerializableItem,CharSequenceItem,ComparableItem,IterableItem
  ))
}
