package tarski

import tarski.AST._
import tarski.Denotations.{BooleanLit, NullLit}
import tarski.Environment.Env
import tarski.Items._
import tarski.Types._

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
  case object EnumBaseItem extends BaseItem {
    def name = "Enum"
    def isClass = true
    def isEnum = true
    def isFinal = false
    private val E = SimpleTypeVar("E")
    def parent = JavaLangPkg
    val tparams = List(E)
    def base = ObjectType
    val supers = List(base,SerializableType,comparable(E))
    val superItems = List(ObjectItem,SerializableItem,ComparableItem)
  }

  // Simple classes
  sealed abstract class SimpleClassItem extends BaseItem {
    override def simple: SimpleType = SimpleType(this, parent.simple)
    override def parent = JavaLangPkg
    override def tparams = Nil
    def isClass = true
    def isEnum = false
    val interfaces: List[ClassType]
    val base: ClassType
    val supers: List[ClassType]
    val superItems: List[ClassItem]
  }

  // Throwable
  case object ThrowableItem extends SimpleClassItem {
    def name = "Throwable"
    val base = ObjectType
    val interfaces = Nil
    val supers = base :: interfaces
    val superItems = supers map (_.item)
    def isFinal = false
  }

  // Iterable
  case object IterableItem extends BaseItem {
    def name = "Iterable"
    def isClass = false
    def isEnum = false
    val base = ObjectType
    private val T = SimpleTypeVar("T")
    def parent = JavaLangPkg
    val tparams = List(T)
    val supers = List(base)
    val superItems = List(ObjectItem)
    def isFinal = false
  }

  // Class String
  case object StringItem extends SimpleClassItem {
    def name = "String"
    val base = ObjectType
    lazy val interfaces = List(comparable(inside),CharSequenceItem.simple,SerializableType)
    lazy val supers = base :: interfaces
    val superItems = List(ObjectItem,ComparableItem,CharSequenceItem,SerializableItem)
    def isFinal = true
  }

  // java.lang.Void
  case object VoidItem extends SimpleClassItem {
    def name = "Void"
    val base = ObjectType
    val interfaces = Nil
    val supers = base :: interfaces
    val superItems = supers map (_.item)
    def isFinal = true
  }

  // Reference wrappers around primitive types
  case object BooleanItem extends SimpleClassItem {
    def name = "Boolean"
    val base = ObjectType
    val interfaces = List(comparable(inside),SerializableType)
    val supers = base :: interfaces
    val superItems = supers map (_.item)
    override def unbox = Some(BooleanType)
    override def unboxesToBoolean = true
    def isFinal = true
  }
  case object CharacterItem extends SimpleClassItem {
    def name = "Character"
    val base = ObjectType
    val interfaces = List(comparable(inside),SerializableType)
    val supers = base :: interfaces
    val superItems = supers map (_.item)
    override def unbox = Some(CharType)
    override def unboxNumeric = Some(CharType)
    override def unboxIntegral = Some(CharType)
    def isFinal = true
  }
  case object NumberItem extends SimpleClassItem {
    def name = "Number"
    val base = ObjectType
    val interfaces = List(SerializableType)
    val supers = base :: interfaces
    val superItems = supers map (_.item)
    def isFinal = false
  }
  sealed abstract class NumberClassItem(val name: Name, val ty: NumType) extends SimpleClassItem {
    val base = NumberItem.simple
    val interfaces = List(comparable(inside),SerializableType)
    val supers = base :: interfaces
    val superItems = supers map (_.item)
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

  object ubVoidItem    extends LangTypeItem { def ty = VoidType }
  object ubBooleanItem extends LangTypeItem { def ty = BooleanType }
  object ubByteItem    extends LangTypeItem { def ty = ByteType }
  object ubShortItem   extends LangTypeItem { def ty = ShortType }
  object ubIntItem     extends LangTypeItem { def ty = IntType }
  object ubLongItem    extends LangTypeItem { def ty = LongType }
  object ubFloatItem   extends LangTypeItem { def ty = FloatType }
  object ubDoubleItem  extends LangTypeItem { def ty = DoubleType }
  object ubCharItem    extends LangTypeItem { def ty = CharType }

  // Literals
  val trueLit = LitValue(BooleanLit(true))
  val falseLit = LitValue(BooleanLit(false))
  val nullLit = LitValue(NullLit)

  // Basic callables for test use
  val ObjectConsItem = NormalConstructorItem(ObjectItem,Nil,Nil)

  // Standard base environment
  val baseEnv = Env(Array(
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
    ObjectConsItem,
    // Literals
    trueLit,falseLit,nullLit
  ))

  // Things that EnvironmentProcessor won't add on its own
  val extraEnv = Env(Array(
    LocalPkg,trueLit,falseLit,nullLit,
    ubVoidItem,ubBooleanItem,ubByteItem,ubShortItem,ubIntItem,ubLongItem,ubFloatItem,ubDoubleItem,ubCharItem))

  // Check that an environment has a unique copy of everything in baseEnv
  def checkEnv(env: Env): Unit = {
    val names = baseEnv.allItems.map(t => t.qualifiedName.get -> t).toMap
    val seen = mutable.Set[String]()
    env.allItems.foreach(t => t.qualifiedName foreach (n => names get n foreach (b => {
      assert(!seen.contains(n),s"Two copies of $n, type ${t.getClass}, t = ${t.hashCode}")
      assert(t eq b,s"Versions of $n in baseEnv (${b.getClass}) and env (${t.getClass}) differ")
      seen += n
    })))
    names foreach {case (n,_) => assert(seen contains n, s"env does not contain $n")}
  }
}