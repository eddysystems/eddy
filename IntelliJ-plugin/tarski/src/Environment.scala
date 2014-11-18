package tarski

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import ambiguity.Utility.notImplemented
import Scores._
import tarski.AST.Name
import tarski.Items._
import tarski.Types._
import tarski.Tokens.show
import tarski.Pretty._

object Environment {
  /**
   * The environment used for name resolution
   */
  case class Env(allthings: List[NamedItem],
                 inScope: Map[NamedItem,Int] = Map(),
                 place: PlaceItem = Base.LocalPkg) extends scala.Serializable {

    assert(place == Base.LocalPkg || allthings.contains(place))
    val things = allthings.filterNot( _.isInstanceOf[NoLookupItem] )

    // Add objects (while filling environment)
    def addObjects(xs: List[NamedItem], is: Map[NamedItem,Int]): Env = {
      // TODO: this is quadratic time
      // TODO: filter identical things (like java.lang.String)
      Env(allthings ++ xs, inScope ++ is, place)
    }

    // Make all items local with priority 1 (for tests)
    def makeAllLocal: Env =
      Env(allthings, things.map(i => (i,1)).toMap)

    // Add local objects (they all appear in inScope with priority 1)
    def addLocalObjects(xs: List[NamedItem]): Env = {
      Env(allthings ++ xs, inScope ++ xs.map((_,1)).toMap, place)
    }

    def move(newPlace: PlaceItem): Env = {
      assert(allthings.contains(newPlace))
      Env(allthings, inScope, newPlace)
    }

    def newVariable(name: String, t: Type): Scored[(Env,LocalVariableItem)] = place match {
      case c: CallableItem =>
        if (inScope.exists( { case (LocalVariableItem(iname,_),_) => iname == name; case _ => false } ))
          fail(s"Invalid new local variable $name: already exists.")
        else {
          val x = LocalVariableItem(name, t)
          single((addObjects(List(x), Map((x,0))),x), Pr.newVariable)
        }
      case _ => fail("Cannot declare local variables outside of methods or constructors.")
    }


    def newField(name: String, t: Type): Scored[(Env,Value)] = place match {
      case c: ClassItem =>
        // if there's already a member of the same name (for our place)
        if (inScope.exists( { case (m: Member,_) => m.container == place && m.name == name; case _ => false } ))
          fail(s"Invalid new field $name: a member with this name already exists.")
        else {
          // TODO: modifiers
          val x = FieldItem(name, t, c)
          single((addObjects(List(x), Map((x,0))),x), Pr.newField)
        }
      case c: InterfaceItem =>
        // if there's already a member of the same name (for our place)
        if (inScope.exists( { case (m: Member,_) => m.container == place && m.name == name; case _ => false } ))
          fail(s"Invalid new field $name: a member with this name already exists.")
        else {
          // TODO: modifiers
          val x = StaticFieldItem(name, t, c)
          single((addObjects(List(x), Map((x,0))),x), Pr.newStaticField)
        }
      case _ => fail("Cannot declare fields outside of class or interface declarations.")
    }

    // Fragile, only use for tests
    def exactLocal(name: String): LocalVariableItem = {
      things collect { case x: LocalVariableItem if x.name == name => x } match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }

    // Check if an item is in scope and not shadowed by another item
    def itemInScope(i: NamedItem): Boolean =
      inScope.contains(i) && !inScope.exists { case (ii,p) => p < inScope.get(i).get && i.name == ii.name }

    // Enter a new block scope
    def pushScope: Env =
      Env(allthings, inScope map { case (i,n) => (i,n+1) }, place)

    // Leave a block scope
    def popScope: Env =
      Env(allthings, inScope collect { case (i,n) if n>1 => (i,n-1) }, place)
  }

  // What could this name be, assuming it is a type?
  // TODO: Handle generics
  def typeScores(name: String)(implicit env: Env): Scored[Type] = {
    def prob(x: NamedItem): (Prob,Type) = {
      val t = x match {
        case x:TypeItem => toType(x,Nil)
        case x:PrimTypeItem => toType(x)
      }
      // TODO other things that influence probability:
      // - kind (Primitive Types are more likely, java.lang types are more likely)
      // - things that are in scope are more likely
      // - things that are almost in scope are more likely (declared in package from which other symbols are imported)
      // - things that appear often in this file/class/function are more likely
      // - things that are declared close by are more likely
      val p = if (x.name == name) Pr.exactType
              else Pr.typoProbability(x.name,name)
      (p,t)
    }
    multiple(env.things collect { case x: TypeItem => prob(x); case x: PrimTypeItem => prob(x) }, s"Type $name not found")
  }

  // What could it be, given it's a callable?
  def callableScores(name: String)(implicit env: Env): Scored[CallableItem] =
    multiple(env.things collect { case x: CallableItem => {
      val p = if (x.name==name) Pr.exactCallable
              else Pr.typoProbability(x.name,name)
      (p,x)
    }}, s"Callable $name not found")

  // What could this be, we know it's a value
  def valueScores(name: String)(implicit env: Env): Scored[Value] =
    multiple(env.things collect { case x: Value => {
      val p = if (x.name==name) Pr.exactValue
              else Pr.typoProbability(x.name,name)
      (p,x)
    }}, s"Value $name not found")

  def objectsOfType(name: String, t: Type)(implicit env: Env): Scored[Value] =
    multiple(env.things collect { case i: Value if isSubtype(i.ourType,t) => {
      val p = if (i.name==name) Pr.exactValueOfType
              else Pr.typoProbability(i.name,name)
      (p,i)
    }}, s"Value $name of type ${show(t)} not found")

  // Same as objectsOfType, but without type arguments
  def objectsOfItem(t: TypeItem)(implicit env: Env): Scored[Value] =
    multiple(env.things collect { case i: Value if isSubitem(i.ourType,t) => {
      (Pr.objectOfItem,i)
    }}, s"Value of item ${show(t)} not found")

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

  // What could this be, assuming it's a callable field of the given type?
  def callableFieldScores(t: Type, name: String)(implicit env: Env): Scored[CallableItem] =
    multiple(env.things collect { case f: CallableItem if memberIn(f,t) => {
      val p = if (f.name==name) Pr.exactCallableField
              else Pr.typoProbability(f.name,name)
      (p,f)
    }}, s"Type ${show(t)} has no callable field $name")

  // What could this name be, assuming it is a member of the given type?
  def fieldScores(t: Type, name: String)(implicit env: Env): Scored[Value with Member] =
    multiple(env.things collect { case f: Value with Member if memberIn(f,t) => {
      val p = if (f.name==name) Pr.exactField
              else Pr.typoProbability(f.name,name)
      (p,f)
    }}, s"Type ${show(t)} has no field $name")

  // what could this be, assuming it's a static member of the given type?
  def staticFieldScores(t: Type, name: String)(implicit env: Env): Scored[StaticValue with Member] =
    multiple(env.things collect {
      case f: StaticFieldItem if memberIn(f,t) => {
        val p = if (f.name==name) Pr.exactStaticField
                else Pr.typoProbability(f.name,name)
        (p,f)
      }
      case f: EnumConstantItem if memberIn(f,t) => {
        val p = if (f.name==name) Pr.exactEnumConst
                else Pr.typoProbability(f.name,name)
        (p,f)
      }
    }, s"Type ${show(t)} has no static field $name")

  // What could this be, assuming it is a type field of the given type?
  // TODO: Handle generics
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    multiple(env.things collect { case f: TypeItem if memberIn(f,t) => {
      val t = toType(f,Nil)
      val p = if (f.name==name) Pr.exactTypeField
              else Pr.typoProbability(f.name,name)
      (p,t)
    }}, s"Type ${show(t)} has no type field $name")

  // The return type of our ambient function
  def returnType(implicit env: Env): Scored[Type] = {
    def die(scope: String) = fail(s"Can't return from $scope scope")
    env.place match {
      case m: MethodItem => single(m.retVal, Pr.certain)
      case m: StaticMethodItem => single(m.retVal, Pr.certain)
      case c: ConstructorItem => single(VoidType, Pr.certain)
      case _:PackageItem => die("package")
      case _:ClassItem => die("class")
      case _:InterfaceItem => die("interface")
    }
  }

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