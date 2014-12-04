package tarski

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import ambiguity.Utility._
import Scores._
import tarski.AST.Name
import tarski.Items._
import tarski.Trie._
import tarski.Types._
import tarski.Tokens.show
import tarski.Pretty._

import scala.collection.mutable

object Environment {
  def itemsToMapList(start: Map[String,List[Item]], bs: Iterable[Item]): Map[String,List[Item]] = {
    val m = mutable.Map[String,List[Item]]()++start
    bs foreach { b => m(b.name) = b :: m.getOrElse(b.name,Nil) }
    m.toMap
  }

  def valuesByItem(start: Map[TypeItem,List[Value]], vs: Iterable[Item]): Map[TypeItem,List[Value]] = {
    val m = mutable.Map[TypeItem,List[Value]]()++start
    def add(s: TypeItem, v: Value) = m(s) = v::m.getOrElse(s,Nil)
    vs foreach {
      case v:Value => v.item match {
        case i:RefTypeItem => supers(i) foreach (s => if (s != ObjectItem) add(s,v))
        case i:LangTypeItem => add(i,v)
      }
      case _ => ()
    }
    m.toMap
  }

  /**
   * The environment used for name resolution
   */
   case class Env(trie: CompactTrie[Item],
                  things: Map[String,List[Item]],
                  inScope: Map[Item,Int],
                  byItem: Map[TypeItem,List[Value]], // Map from item to values with matching type
                  place: PlaceItem,
                  inside_breakable: Boolean,
                  inside_continuable: Boolean,
                  labels: List[String]) extends scala.Serializable {

    // add some new things to an existing environment, and optionally change place
    def this(base: Env, newthings: Iterable[Item], newScope: Map[Item,Int],
             place: PlaceItem,
             inside_breakable: Boolean,
             inside_continuable: Boolean,
             labels: List[String]) = {
      this(base.trie.add(newthings), itemsToMapList(base.things,newthings), base.inScope ++ newScope,
           valuesByItem(base.byItem,newthings), place, inside_breakable, inside_continuable, labels)
    }

    // make an environment from a list of items
    def this(things: Iterable[Item], inScope: Map[Item,Int] = Map(),
             place: PlaceItem = Base.LocalPkg,
             inside_breakable: Boolean = false,
             inside_continuable: Boolean = false,
             labels: List[String] = Nil) = {
      this(new CompactTrie[Item](things)(_.name), itemsToMapList(Map.empty,things), inScope,
           valuesByItem(Map.empty,things), place, inside_breakable, inside_continuable, labels)
    }

    // minimum probability before an object is considered a match for a query
    val minimumProbability = Prob(.01)

    assert( place == Base.LocalPkg || things.getOrElse(place.name, Nil).contains(place) )

    // Add objects
    def addObjects(xs: Iterable[Item], is: Map[Item,Int]): Env = {
      // TODO: filter identical things (like java.lang.String)
      new Env(this, xs, is, place, inside_breakable, inside_continuable, labels)
    }

    // Add local objects (they all appear in inScope with priority 1)
    def addLocalObjects(xs: List[Item]): Env =
      addObjects(xs, (xs map {(_,1)}).toMap)

    def move(newPlace: PlaceItem, inside_breakable: Boolean, inside_continuable: Boolean, labels: List[String]): Env = {
      Env(trie, things, inScope, byItem, newPlace, inside_breakable, inside_continuable, labels)
    }

    def newVariable(name: String, t: Type, isFinal: Boolean): Scored[(Env,LocalVariableItem)] = place match {
      case c: CallableItem =>
        if (inScope.exists( { case (v:LocalVariableItem,_) => v.name == name; case _ => false } ))
          fail(s"Invalid new local variable $name: already exists.")
        else {
          val x = LocalVariableItem(name,t,isFinal)
          single((addObjects(List(x), Map((x,0))),x), Pr.newVariable)
        }
      case _ => fail("Cannot declare local variables outside of methods or constructors.")
    }

    def newField(name: String, t: Type, isStatic: Boolean, isFinal: Boolean): Scored[(Env,Value)] = place match {
      case c: ClassItem =>
        // if there's already a member of the same name (for our place)
        if (inScope.exists( { case (m: Member,_) => m.parent == place && m.name == name; case _ => false } ))
          fail(s"Invalid new field $name: a member with this name already exists.")
        else {
          val x = if (isStatic) StaticFieldItem(name,t,c,isFinal)
                  else                FieldItem(name,t,c,isFinal)
          val p = if (isStatic) Pr.newStaticField else Pr.newField
          single((addObjects(List(x),Map((x,0))),x),p)
        }
      case _ => fail("Cannot declare fields outside of class or interface declarations.")
    }

    // Fragile, only use for tests
    def exactLocal(name: String): LocalVariableItem = {
      things.getOrElse(name, Nil) collect { case x: LocalVariableItem => x } match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }

    // Check if an item is in scope and not shadowed by another item
    def itemInScope(i: Item): Boolean =
      inScope.contains(i) && !inScope.exists { case (ii,p) => p < inScope.get(i).get && i.name == ii.name }

    // Enter a new block scope
    def pushScope: Env =
      Env(trie, things, inScope map { case (i,n) => (i,n+1) },
          byItem, place, inside_breakable, inside_continuable, labels)

    // Leave a block scope
    def popScope: Env =
      Env(trie, things, inScope collect { case (i,n) if n>1 => (i,n-1) },
          byItem, place, inside_breakable, inside_continuable, labels)

    // get typo probabilities for string queries
    def query(typed: String): List[Alt[Item]] = {
      val e = typed.length * Pr.typingErrorRate
      val maxErrors = Pr.poissonQuantile(e, minimumProbability) // this never discards anything because it has too few errors
      levenshteinLookup(trie, typed, maxErrors) map {
        case (d,item) => Alt(Pr.poissonPDF(e,math.ceil(d).toInt), item)
      } filter {
        case Alt(p,item) => p > minimumProbability
      }
    }

    def exactQuery(typed: String): List[Alt[Item]] = {
      val e = typed.length * Pr.typingErrorRate
      val p = Pr.poissonPDF(e,0)
      things.getOrElse(typed, Nil) map { Alt(p, _) }
    }

    def combinedQuery[A](typed: String, exactProb: Prob, filter: PartialFunction[Item,A], error: String): Scored[A] = {
      val _f = Function.unlift( (x:Alt[Item]) => { if (filter.isDefinedAt(x.x)) Some(Alt(x.p, filter.apply(x.x))) else None } )
      multiples(exactQuery(typed) collect _f map { case Alt(p,t) => Alt(exactProb,t) },
               { query(typed) collect _f map { case Alt(p,t) => Alt((1-exactProb)*p,t) } },
               error)
    }
  }

  // What could this name be, assuming it is a type?
  // TODO: Handle generics
  def typeScores(name: String)(implicit env: Env): Scored[Type] = {
    // TODO other things that influence probability:
    // - kind (Primitive Types are more likely, java.lang types are more likely)
    // - things that are in scope are more likely
    // - things that are almost in scope are more likely (declared in package from which other symbols are imported)
    // - things that appear often in this file/class/function are more likely
    // - things that are declared close by are more likely
    env.combinedQuery(name, Pr.exactType, { case t:TypeItem => t.raw }, s"Type $name not found")
  }

  // What could it be, given it's a callable?
  def callableScores(name: String)(implicit env: Env): Scored[CallableItem] =
    env.combinedQuery(name, Pr.exactCallable, { case t:CallableItem => t }, s"Callable $name not found")

  // What could this be, we know it's a value
  def valueScores(name: String)(implicit env: Env): Scored[Value] =
    env.combinedQuery(name, Pr.exactValue, { case t:Value => t }, s"Value $name not found")

  // Same as objectsOfType, but without type arguments
  def objectsOfItem(t: TypeItem)(implicit env: Env): Scored[Value] =
    uniform(Pr.objectOfItem, env.byItem.getOrElse(t,Nil), s"Value of item ${show(t)} not found")

  // Does a member belong to a type?
  def memberIn(f: Item, t: Type): Boolean = f match {
    case f: ClassMember => {
      val p = f.parent
      supers(t) exists (_.item == p)
    }
    case _ => false
  }

  // Assuming a member belongs to a type, what is its fully applied type?
  def typeIn(f: TypeItem, t: Type): Type = f match {
    case f: ClassMember => {
      def p = f.parent
      collectOne(supers(t)){
        case t:ClassType if t.item==p => f.inside.substitute(t.env)
      }.getOrElse(throw new RuntimeException("typeIn didn't find parent"))
    }
    case _ => throw new RuntimeException("typeIn didn't find parent")
  }

  // Does an item declare a member of the given name
  def declaresName(i: Item, name: Name)(implicit env: Env): Boolean = {
    env.things.getOrElse(name, Nil) exists { case f: Member if f.parent == i => true; case _ => false }
  }

  def shadowedInSubType(i: Member, t: RefType)(implicit env: Env): Boolean = {
    i.parent match {
      case c: RefTypeItem => {
        assert(isSubitem(t,c))
        t.item match {
          case t if c == t => false
          case t: ClassItem => declaresName(t, i.name) || shadowedInSubType(i, t.base)
          case _ => false
        }
      }
      case _:PackageItem => false // member of package, no subtypes of packages, we're safe
      case v: Value => throw new RuntimeException(s"container of $i cannot be a value: $v")
      case _ => notImplemented // TODO: there can be local classes in methods
    }
  }

  // What could this be, assuming it's a callable field of the given type?
  def callableFieldScores(t: Type, name: String)(implicit env: Env): Scored[CallableItem] =
    env.combinedQuery(name, Pr.exactCallableField, { case f:CallableItem if memberIn(f,t) => f }, s"Type ${show(t)} has no callable field $name")

  // What could this name be, assuming it is a member of the given type?
  def fieldScores(t: Type, name: String)(implicit env: Env): Scored[Value with Member] =
    env.combinedQuery(name, Pr.exactField, { case f: Value with Member if memberIn(f,t) => f }, s"Type ${show(t)} has no field $name")

  // what could this be, assuming it's a static member of the given type?
  def staticFieldScores(t: Type, name: String)(implicit env: Env): Scored[StaticValue with Member] =
    env.combinedQuery(name, Pr.exactStaticField, { case f:StaticFieldItem if memberIn(f,t) => f case f: EnumConstantItem if memberIn(f,t) => f },
                      s"Type ${show(t)} has no static field $name")

  // What could this be, assuming it is a type field of the given type?
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    env.combinedQuery(name, Pr.exactTypeField, { case f: TypeItem if memberIn(f,t) => typeIn(f,t) }, s"Type ${show(t)} has no type field $name")

  // The return type of our ambient function
  def returnType(implicit env: Env): Scored[Type] = {
    def die(scope: String) = fail(s"Can't return from $scope scope")
    env.place match {
      case m: MethodItem => single(m.retVal, Pr.certain)
      case m: StaticMethodItem => single(m.retVal, Pr.certain)
      case c: ConstructorItem => single(VoidType, Pr.certain)
      case _:PackageItem => die("package")
      case _:ClassItem => die("class or interface")
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