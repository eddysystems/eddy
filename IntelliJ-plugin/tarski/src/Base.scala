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
    val T = TypeParamItem("T")
    NormalInterfaceItem("Comparable",JavaLangPkg,List(T))
  }
  private def comparable(t: RefType) = GenericInterfaceType(ComparableItem,List(t))
  private def comparable(c: ClassItem) = GenericInterfaceType(ComparableItem,List(SimpleClassType(c)))

  // Class Enum
  object EnumBaseItem extends ClassItem("Enum") {
    private val E = TypeParamItem("E")
    def container = JavaLangPkg
    def params = List(E)
    def base = ObjectType
    def implements = List(SerializableType,comparable(ParamType(E)))
  }

  // Simple classes
  sealed abstract class SimpleClassItem(override val name: Name) extends ClassItem(name) {
    override def container = JavaLangPkg
    override def params = Nil
  }

  // Throwable
  object ThrowableItem extends SimpleClassItem("Throwable") {
    def base = ObjectType
    def implements = Nil
  }

  // Iterable
  object IterableItem extends InterfaceItem("Iterable") {
    private val T = TypeParamItem("T")
    def container = JavaLangPkg
    def params = List(T)
    def bases = Nil
  }

  // Class String
  object StringItem extends SimpleClassItem("String") {
    def base = ObjectType
    def implements = List(comparable(this),SimpleInterfaceType(CharSequenceItem),SerializableType)
  }

  // Reference wrappers around primitive types
  object BooleanItem extends SimpleClassItem("Boolean") {
    def base = ObjectType
    def implements = List(comparable(this),SerializableType)
  }
  object CharacterItem extends SimpleClassItem("Character") {
    def base = ObjectType
    def implements = List(comparable(this),SerializableType)
  }
  object NumberItem extends SimpleClassItem("Number") {
    def base = ObjectType
    def implements = List(SerializableType)
  }
  sealed abstract class NumberClassItem(name: Name) extends SimpleClassItem(name) {
    def base = SimpleClassType(NumberItem)
    def implements = List(comparable(this),SerializableType)
  }
  object ByteItem    extends NumberClassItem("Byte")
  object ShortItem   extends NumberClassItem("Short")
  object IntegerItem extends NumberClassItem("Integer")
  object LongItem    extends NumberClassItem("Long")
  object FloatItem   extends NumberClassItem("Float")
  object DoubleItem  extends NumberClassItem("Double")

  // Basic callables for test use
  val ObjectConsItem = ConstructorItem(ObjectItem,Nil,Nil)

  // Standard base environment
  val baseEnv = Env(List(
    // Packages
    JavaLangPkg,JavaIoPkg,LocalPkg,
    // Classes
    ObjectItem,ObjectConsItem,
    EnumBaseItem,ThrowableItem,StringItem,BooleanItem,CharacterItem,
    NumberItem,ByteItem,ShortItem,IntegerItem,LongItem,FloatItem,DoubleItem,
    // Interfaces
    CloneableItem,SerializableItem,CharSequenceItem,ComparableItem,IterableItem
  ))
}
