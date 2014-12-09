package tarski

import tarski.AST.Name
import tarski.Items._
import tarski.Types._
import tarski.Environment.Env
import scala.collection.mutable

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
  case object EnumBaseItem extends ClassItem {
    def name = "Enum"
    def isClass = true
    def isEnum = true
    def isFinal = false
    private val E = SimpleTypeVar("E")
    def parent = JavaLangPkg
    def tparams = List(E)
    def base = ObjectType
    def implements = List(SerializableType,comparable(E))
  }

  // Simple classes
  sealed abstract class SimpleClassItem extends ClassItem {
    override def simple: SimpleType = SimpleType(this, parent.simple)
    override def parent = JavaLangPkg
    override def tparams = Nil
    def isClass = true
    def isEnum = false
  }

  // Throwable
  case object ThrowableItem extends SimpleClassItem {
    def name = "Throwable"
    def base = ObjectType
    def implements = Nil
    def isFinal = false
  }

  // Iterable
  case object IterableItem extends ClassItem {
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
  case object StringItem extends SimpleClassItem {
    def name = "String"
    def base = ObjectType
    def implements = List(comparable(inside),CharSequenceItem.simple,SerializableType)
    def isFinal = true
  }

  // java.lang.Void
  case object VoidItem extends SimpleClassItem {
    def name = "Void"
    def base = ObjectType
    def implements = Nil
    def isFinal = true
  }

  // Reference wrappers around primitive types
  case object BooleanItem extends SimpleClassItem {
    def name = "Boolean"
    def base = ObjectType
    def implements = List(comparable(inside),SerializableType)
    override def unbox = Some(BooleanType)
    override def unboxesToBoolean = true
    def isFinal = true
  }
  case object CharacterItem extends SimpleClassItem {
    def name = "Character"
    def base = ObjectType
    def implements = List(comparable(inside),SerializableType)
    override def unbox = Some(CharType)
    override def unboxNumeric = Some(CharType)
    override def unboxIntegral = Some(CharType)
    def isFinal = true
  }
  case object NumberItem extends SimpleClassItem {
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
  case object ByteItem    extends NumberClassItem("Byte",ByteType)
  case object ShortItem   extends NumberClassItem("Short",ShortType)
  case object IntegerItem extends NumberClassItem("Integer",IntType)
  case object LongItem    extends NumberClassItem("Long",LongType)
  case object FloatItem   extends NumberClassItem("Float",FloatType)
  case object DoubleItem  extends NumberClassItem("Double",DoubleType)

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
    // Primitive types
    ubVoidItem,ubBooleanItem,ubByteItem,ubShortItem,ubIntItem,ubLongItem,ubFloatItem,ubDoubleItem,ubCharItem,
    // Classes
    ObjectItem,VoidItem,
    EnumBaseItem,ThrowableItem,StringItem,BooleanItem,CharacterItem,
    NumberItem,ByteItem,ShortItem,IntegerItem,LongItem,FloatItem,DoubleItem,
    // Interfaces
    CloneableItem,SerializableItem,CharSequenceItem,ComparableItem,IterableItem,
    // Constructors
    ObjectConsItem
  ))

  // Things that EnvironmentProcessor won't add on its own
  val extraEnv = new Env(List(
    LocalPkg,
    ubVoidItem,ubBooleanItem,ubByteItem,ubShortItem,ubIntItem,ubLongItem,ubFloatItem,ubDoubleItem,ubCharItem))

  // Map from qualified names to base environment entries
  val baseQualifiedNames: Map[String,Item] = baseEnv.items.map(t => t.qualifiedName.get -> t).toMap

  // Check that an environment has a unique copy of everything in baseEnv
  def checkEnv(env: Env): Unit = {
    val seen = mutable.Set[String]()
    env.items.foreach(t => t.qualifiedName foreach (n => baseQualifiedNames get n foreach (b => {
      assert(!seen.contains(n),s"Two copies of $n, type ${t.getClass}, t = ${t.hashCode}, CharSequenceItem: ${CharSequenceItem.hashCode}")
      assert(t eq b,s"Versions of $n in baseEnv (${b.getClass}) and env (${t.getClass}) differ")
      seen += n
    })))
    baseQualifiedNames foreach {case (n,_) => assert(seen contains n, s"env does not contain $n")}
  }
}
