package tarski

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import ambiguity.Utility.notImplemented
import Scores._
import tarski.AST.Name
import tarski.Denotations.Exp
import tarski.Items._
import tarski.Types._
import tarski.Tokens.show
import tarski.Pretty._

object Environment {
  /**
   * The environment used for name resolution
   */
  case class Env(allthings: List[NamedItem], inScope: Map[NamedItem,Int] = Map[NamedItem,Int](), place: NamedItem = Base.LocalPkg) extends scala.Serializable {

    assert(place == Base.LocalPkg || allthings.contains(place))
    val things = allthings.filterNot( _.isInstanceOf[NoLookupItem] )

    // add objects (while filling environment)
    def addObjects(xs: List[NamedItem], is: Map[NamedItem,Int]): Env = {
      // TODO: this is quadratic time
      // TODO: filter identical things (like java.lang.String)
      Env(allthings ++ xs, inScope ++ is, place)
    }

    // makes all items local with priority 1 (for tests)
    def makeAllLocal(): Env = {
      Env(allthings, things.map( i => (i,1)).toMap)
    }

    // add local objects (they all appear in inScope with priority 1)
    def addLocalObjects(xs: List[NamedItem]): Env = {
      Env(allthings ++ xs, inScope ++ xs.map((_,1)).toMap, place)
    }

    def move(newPlace: NamedItem): Env = {
      assert(allthings.contains(newPlace))
      Env(allthings, inScope, newPlace)
    }

    def newVariable(name: String, t: Type): Scored[(Env,LocalVariableItem)] =
      if (inScope.exists( { case (LocalVariableItem(iname,_),1) => iname == name; case _ => false } )) // shadowing == 1 => local variable in this block
        fail(s"Invalid new local variable $name: already exists.")
      else {
        val x = LocalVariableItem(name, t)
        single((addObjects(List(x), Map((x,1))),x))
      }

    // fragile, only use for tests
    def exactLocal(name: String): LocalVariableItem = {
      things collect { case x: LocalVariableItem if x.name == name => x } match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }

    // check if an item is in scope and not shadowed by another item
    def itemInScope(i: NamedItem): Boolean =
      inScope.contains(i) && !inScope.exists { case (ii,p) => p < inScope.get(i).get && i.name == ii.name }

  }

  // Fuzzy Query interface

  // What could this name be?
  def scores(name: String)(implicit env: Env): Scored[NamedItem] =
    simple(env.things.filter(_.name == name), s"Item $name not found")

  // What could this name be, assuming it is a type?
  // TODO: Handle generics
  def typeScores(name: String)(implicit env: Env): Scored[Type] =
    simple(env.things collect { case x: TypeItem if x.name==name => toType(x,Nil) },
           s"Type $name not found")

  // Objects of a given type (or subtypes thereof)
  def objectsOfType(t: Type)(implicit env: Env): Scored[Value] =
    simple(env.things collect { case i: Value if isSubtype(i.ourType,t) => i },
           s"Value of type ${show(t)} not found")
  def objectsOfType(name: String, t: Type)(implicit env: Env): Scored[Value] =
    simple(env.things collect { case i: Value if isSubtype(i.ourType,t) && i.name == name => i },
           s"Value $name of type ${show(t)} not found")

  // Same as objectsOfType, but without type arguments
  def objectsOfItem(t: TypeItem)(implicit env: Env): Scored[Value] =
    simple(env.things collect { case i: Value if isSubitem(i.ourType,t) => i },
           s"Value of item ${show(t)} not found")

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
        case ObjectType => itemHas(ObjectItem)
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

  // does an item declare a member of the given name
  def declaresName(i: NamedItem, name: Name)(implicit env: Env): Boolean = {
    env.things.exists({ case f: Member if f.container == i && f.name == name => true; case _ => false })
  }

  def shadowedInSubType(i: Member, t: RefType)(implicit env: Env): Boolean = {
    i.container match {
      case c: RefTypeItem => {
        assert(isSubitem(t,c))
        toItem(t) match {
          case Some(ti) if c == ti => false
          case Some(ti: ClassItem) => declaresName(ti, i.name) || shadowedInSubType(i, ti.base)
          case None => false
        }
      }
      case PackageItem(_,_) => false // member of package, no subtypes of packages, we're safe
      case v: Value => throw new RuntimeException(s"container of $i cannot be a value: $v")
      case _ => notImplemented // TODO: there can be local classes in methods
    }
  }

  // What could this name be, assuming it is a field of the given type?
  def fieldScores(t: Type, name: String)(implicit env: Env): Scored[NamedItem] =
    simple(env.things.filter(f => f.name == name && memberIn(f,t)),
           s"Type ${show(t)} has no field $name")

  // What could this be, assuming it is a type field of the given type?
  // TODO: Handle generics
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    simple(env.things collect { case f: TypeItem if f.name==name && memberIn(f,t) => toType(f,Nil) },
           s"Type ${show(t)} has no type field $name")

  // what could this name be, assuming it is an annotation
  def annotationScores(name: String)(implicit env: Env): Scored[AnnotationItem] =
    simple(env.things.collect({case a: AnnotationItem if a.name==name => a}),
           "Annotation @$name not found")

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