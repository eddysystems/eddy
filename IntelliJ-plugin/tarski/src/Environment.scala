package tarski

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import Scores._
import tarski.Items._

object Environment {
  /**
   * The environment used for name resolution
   */
  case class Env(things: List[NamedItem]) extends scala.Serializable {
    // used on plugin side to fill in data
    def addObjects(xs: List[NamedItem]): Env = {
      // TODO: this is quadratic time due to order, but order for now is important
      // TODO: filter identical things (like java.lang.String)
      Env(things ++ xs)
    }
    
    def newVariable(name: String, t: Type): Scored[(Env,LocalVariableItem)] =
      if (this.things.exists(_.relativeName == name))
        fail
      else {
        val x = LocalVariableItem(name, t)
        single((addObjects(List(x)),x))
      }

    // fragile, only use for tests
    def exactLocal(name: String): LocalVariableItem = {
      things collect { case x: LocalVariableItem if x.name == name => x } match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }
  }

  // Fuzzy Query interface

  // What could this name be?
  def scores(name: String)(implicit env: Env): Scored[EnvItem] =
    simple(env.things.filter(_.name == name))

  // What could this name be, assuming it is a type?
  def typeScores(name: String)(implicit env: Env): Scored[Type] =
    simple(env.things.collect({case x: Type if x.name==name => x}))

  // objects of a given type (name "" matches all objects)
  def objectsOfType(name: String, t: Type)(implicit env: Env): Scored[EnvItem] =
    simple(env.things.filter({case i: Value if name == "" || i.name == name => true }))

  // Does a member belong to a type?
  def memberIn(f: EnvItem, t: Type): Boolean = f match {
    case m: Member => m.containing == t // TODO: Subtypes are not handled here
    case _ => false
  }

  // What could this name be, assuming it is a field of the given type?
  def fieldScores(t: Type, name: String)(implicit env: Env): Scored[EnvItem] =
    simple(env.things.filter(f => f.name == name && memberIn(f,t)))

  // What could this be, assuming it is a type field of the given type?
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    simple(env.things.collect({case f: Type if f.name==name && memberIn(f,t) => f}))

  // what could this name be, assuming it is an annotation
  def annotationScores(name: String)(implicit env: Env): Scored[AnnotationItem] =
    simple(env.things.collect({case a: AnnotationItem if a.name==name => a}))

  def envToFile(env: Env, name: String): Unit = {
    val os = new FileOutputStream(name)
    val oos = new ObjectOutputStream(os)
    oos.writeObject(env)
    oos.close()
    os.close()
  }

  def envFromFile(name: String): Env = {
    val is = new FileInputStream(name)
    val ois = new ObjectInputStream(is)
    val env = ois.readObject().asInstanceOf[Env]
    ois.close()
    is.close()
    env
  }

  val baseEnvironment = Env(List(BooleanType, IntType, FloatType, LongType, DoubleType, CharType,
    JavaLangPkg, JavaIoPkg, LocalPkg,
    ObjectType, Serializable, CharSequence,
    ComparableEnum, ComparableString, ComparableBoolean, ComparableCharacter, ComparableByte, ComparableShort, ComparableInteger, ComparableLong, ComparableFloat, ComparableDouble,
    NumberType, EnumBaseType, StringType,
    BooleanRefType, CharRefType, ByteRefType, ShortRefType, IntRefType, LongRefType, FloatRefType, DoubleRefType
  ))
}