package tarski

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import Scores._
import tarski.Items._
import tarski.Types._

object Environment {
  /**
   * The environment used for name resolution
   */
  case class Env(allthings: List[NamedItem]) extends scala.Serializable {

    val things = allthings.filterNot( _.isInstanceOf[NoLookupItem] )

    // used on plugin side to fill in data
    def addObjects(xs: List[NamedItem]): Env = {
      // TODO: this is quadratic time due to order, but order is important for shadowing for now
      // TODO: filter identical things (like java.lang.String)
      Env(things ++ xs)
    }
    
    def newVariable(name: String, t: Type): Scored[(Env,LocalVariableItem)] =
      if (this.things.exists(_.name == name)) // TODO: Fix name handling
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
  def scores(name: String)(implicit env: Env): Scored[NamedItem] =
    simple(env.things.filter(_.name == name))

  // What could this name be, assuming it is a type?
  // TODO: Handle generics
  def typeScores(name: String)(implicit env: Env): Scored[Type] =
    simple(env.things collect { case x: TypeItem if x.name==name => toType(x) })

  // Objects of a given type (or subtypes thereof)
  def objectsOfType(t: Type)(implicit env: Env): Scored[Value] =
    simple(env.things collect { case i: Value if isSubtype(i.ourType,t) => i })
  def objectsOfType(name: String, t: Type)(implicit env: Env): Scored[Value] =
    simple(env.things collect { case i: Value if isSubtype(i.ourType,t) && i.name == name => i })

  // Does a member belong to a type?
  def memberIn(f: Item, t: Type): Boolean = f match {
    case m: ClassMember => {
      val d = m.container
      def itemHas(t: TypeItem): Boolean = d == t || (t match {
        case ObjectItem => false
        case t: InterfaceItem => t.bases exists typeHas
        case t: ClassItem => typeHas(t.base) || t.implements.exists(typeHas)
      })
      def typeHas(t: RefType): Boolean = t match {
        case t: InterfaceType => itemHas(t.d)
        case t: ClassType => itemHas(t.d)
        case ArrayType(_) => false // TODO: Arrays have lengths
        case ObjectType => false // TODO: Does Object have fields?
        case NullType|ErrorType(_)|ParamType(_) => false
        case IntersectType(ts) => ts exists typeHas
      }
      t match {
        case t: RefType => typeHas(t)
        case _ => false
      }
    }
    case _ => false
  }

  // What could this name be, assuming it is a field of the given type?
  def fieldScores(t: Type, name: String)(implicit env: Env): Scored[NamedItem] =
    simple(env.things.filter(f => f.name == name && memberIn(f,t)))

  // What could this be, assuming it is a type field of the given type?
  // TODO: Handle generics
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    simple(env.things collect { case f: TypeItem if f.name==name && memberIn(f,t) => toType(f) })

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
}