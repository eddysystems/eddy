package tarski

import Semantics.Score
import tarski.AST.{UnaryOp, BinaryOp, Name}

import scala.collection.mutable

object Environment {

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

  sealed abstract class Modifier
  object PublicModifier extends Modifier
  object ProtectedModifier extends Modifier
  object PrivateModifier extends Modifier
  object StaticModifier extends Modifier
  object FinalModifier extends Modifier
  object StrictfpModifier extends Modifier
  object TransientModifier extends Modifier
  object VolatileModifier extends Modifier
  object SynchronizedModifier extends Modifier

  // EnvItems that are a type-like thing
  sealed abstract class TypeItem(name: Name) extends NamedItem(name)

  // NamedItems that have a type
  sealed abstract class HasTypeItem(name: Name, val ourType: TypeItem) extends NamedItem(name)

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

  sealed abstract class ClassItem(name: Name) extends TypeItem(name)
  sealed abstract class EnumItem(name: Name) extends ClassItem(name)
  sealed abstract class InterfaceItem(name: Name) extends ClassItem(name)

  class ArrayTypeItem[A <: TypeItem](val inner: A, val dims: Int) extends TypeItem(inner.name + "[]") {
    override def qualifiedName = inner.qualifiedName + "[]"
    override def relativeName = inner.relativeName + "[]"
  }

  // things with types
  sealed abstract class FieldItem(name: Name, ourType: TypeItem, val cls: ClassItem) extends HasTypeItem(name, ourType)
  sealed abstract class ParameterItem(name: Name, ourType: TypeItem) extends HasTypeItem(name, ourType)
  sealed abstract class LocalVariableItem(name: Name, ourType: TypeItem) extends HasTypeItem(name, ourType)
  sealed class EnumConstantItem(name: Name, val ourType: EnumItem) extends NamedItem(name) {
    def qualifiedName = ourType.qualifiedName + '.' + name
    def relativeName = ourType.relativeName + '.' + name
  }

  // callables
  sealed abstract class MethodItem(name: Name, val retVal: TypeItem, paramTypes: List[TypeItem]) extends CallableItem(name, paramTypes)
  sealed class ConstructorItem(val cls: ClassItem, paramTypes: List[TypeItem]) extends CallableItem(cls.name, paramTypes) {
    def qualifiedName = cls.qualifiedName + name
    def relativeName = cls.qualifiedName + name
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


  /**
   * Contains the environment used for name resolution
   */
  class JavaEnvironment {

    // object storage, comes pre-spawned with Java's basic types
    var things: mutable.MutableList[NamedItem] = mutable.MutableList(BooleanItem, IntItem, FloatItem, LongItem, DoubleItem, CharItem)

    // used on plugin side to fill in data
    def addObject(thing: NamedItem): Unit = {
      //println("environment object " + thing)
      things += thing
    }

    // whether from can be implicitly converted to to
    def convertibleTo(from: TypeItem, to: TypeItem): Boolean = {
      // TODO
      from == to
    }

    // whether from can be explicitly cast to to
    def castableTo(from: TypeItem, to: TypeItem): Boolean = {
      // TODO
      from == to
    }

    def operatorLegal(op: BinaryOp, t0: TypeItem, t1: TypeItem): Boolean = {
      // TODO
      t0 == t1
    }

    def operatorLegal(op: UnaryOp, t: TypeItem): Boolean = {
      // TODO
      true
    }

    def expressionType(op: BinaryOp, t0: TypeItem, t1: TypeItem): TypeItem = {
      // TODO
      t0
    }

    // get an object
    def getObjectByName[A <: NamedItem](name: String): A = things.find( x => x.isInstanceOf[A] && x.name == name ).orNull.asInstanceOf[A]

    // Fuzzy Query interface

    // what could this name be?
    def getScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter( _.name == name ).map( x => (new Score(1.0f), x) )
    }

    // what could this name be, assuming it is a type?
    def getTypeScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter( x => x.isInstanceOf[TypeItem] && x.name == name ).map((new Score(1.0f), _))
    }

    // what could this be, assuming it is a type field of the given type?
    def getTypeFieldScores(t: TypeItem, name: String): List[(Score, EnvItem)] = {
      // TODO: this should take into account containers
      things.toList.filter( x => x.isInstanceOf[TypeItem] && x.name == name ).map((new Score(1.0f), _))
    }

    // what could this name be, assuming it is an annotation
    def getAnnotationScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter(x => x.isInstanceOf[AnnotationItem] && x.name == name).map((new Score(1.0f), _))
    }
  }

}