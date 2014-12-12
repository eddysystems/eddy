package tarski

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import ambiguity.JavaUtils._
import ambiguity.Utility._
import tarski.AST._
import tarski.Items._
import tarski.Scores._
import tarski.Tokens._
import tarski.Tries._
import tarski.Types._

/**
 * Created by martin on 11.12.14.
 */
object Environment {
  // Minimum probability before an object is considered a match for a query
  val minimumProbability = Prob(.01)

  // Lookup a string, approximately
  private def typoQuery(trie: Trie[Item], typed: String): List[Alt[Item]] = {
    val expected = typed.length * Pr.typingErrorRate
    val maxErrors = Pr.poissonQuantile(expected,minimumProbability) // Never discards anything because it has too few errors
    levenshteinLookup(trie,typed,maxErrors,expected,minimumProbability)
  }

  // Information about where we are
  case class PlaceInfo(place: PlaceItem,
                       breakable: Boolean = false,
                       continuable: Boolean = false,
                       labels: List[String] = Nil)
  val localPlace = PlaceInfo(Base.LocalPkg)

  // An environment for name resolution
  abstract class Env {
    // Where we are
    def scope: Map[Item,Int]
    def place: PlaceInfo
    def move(to: PlaceInfo): Env

    // Add more objects
    def extend(things: Array[Item], scope: Map[Item,Int]): Env

    // Add local objects (they all appear in inScope with priority 1)
    def extendLocal(things: Array[Item]): Env =
      extend(things,(things map ((_,1))).toMap)

    // Is an item in scope?
    def inScope(i: Item): Boolean

    // Enter and leave block scopes
    def pushScope: Env
    def popScope: Env

    // Add variables and fields
    def newVariable(name: String, t: Type, isFinal: Boolean): Scored[(Env,LocalVariableItem)]
    def newField(name: String, t: Type, isStatic: Boolean, isFinal: Boolean): Scored[(Env,Value)]

    // Get exact and typo probabilities for string queries
    def query(typed: String): List[Alt[Item]]
    def exactQuery(typed: String): List[Alt[Item]]
    def combinedQuery[A](typed: String, exactProb: Prob, filter: PartialFunction[Item,A], error: String): Scored[A]

    // Lookup by type.item
    def byItem(t: TypeItem): Scored[Value]

    // Fragile or slow, only use for tests
    def exactLocal(name: String): LocalVariableItem
    def allItems: Array[Item]
  }

  // Constructors for Env
  object Env {
    def apply(items: Array[Item], scope: Map[Item,Int] = Map.empty, place: PlaceInfo = localPlace): Env =
      scoped("make env",
        TwoEnv(Trie(items),Trie.empty,
               valuesByItem(items),new java.util.HashMap[TypeItem,Array[Value]](),
               scope,place))
  }

  // Store two tries and two byItems: one large one for globals, one small one for locals.
  case class TwoEnv(private val trie0: Trie[Item],
                    private val trie1: Trie[Item],
                    private val byItem0: java.util.Map[TypeItem,Array[Value]],
                    private val byItem1: java.util.Map[TypeItem,Array[Value]],
                    scope: Map[Item,Int],
                    place: PlaceInfo) extends Env {
    // Slow, use only for tests
    def allItems: Array[Item] = trie0.values++trie1.values

    // Add some new things to an existing environment
    def extend(things: Array[Item], scope: Map[Item,Int]) =
      TwoEnv(trie0,trie1++things,
             byItem0,valuesByItem(trie1.values++things),
             scope++scope,place)

    def move(to: PlaceInfo) =
      TwoEnv(trie0,trie1,byItem0,byItem1,scope,to)

    def newVariable(name: String, t: Type, isFinal: Boolean) = place.place match {
      case c: CallableItem =>
        if (scope exists { case (v:LocalVariableItem,_) => v.name == name; case _ => false })
          fail(s"Invalid new local variable $name: already exists.")
        else {
          val x = LocalVariableItem(name,t,isFinal)
          single((extend(Array(x),Map((x,0))),x), Pr.newVariable)
        }
      case _ => fail("Cannot declare local variables outside of methods or constructors.")
    }

    def newField(name: String, t: Type, isStatic: Boolean, isFinal: Boolean) = place.place match {
      case c: ClassItem =>
        // Check if there's already a member of the same name (for our place)
        if (scope exists { case (m: Member,_) => m.parent == place && m.name == name; case _ => false })
          fail(s"Invalid new field $name: a member with this name already exists.")
        else {
          val x = if (isStatic) NormalStaticFieldItem(name,t,c,isFinal)
                  else                NormalFieldItem(name,t,c,isFinal)
          val p = if (isStatic) Pr.newStaticField else Pr.newField
          single((extend(Array(x),Map((x,0))),x),p)
        }
      case _ => fail("Cannot declare fields outside of class or interface declarations.")
    }

    // Fragile, only use for tests
    def exactLocal(name: String): LocalVariableItem = {
      def options(t: Trie[Item]) = t exact name collect { case x: LocalVariableItem => x }
      options(trie0)++options(trie1) match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }

    // Check if an item is in scope and not shadowed by another item
    def inScope(i: Item): Boolean =
      scope.contains(i) && !scope.exists { case (ii,p) => p < scope.get(i).get && i.name == ii.name }

    // Enter a new block scope
    def pushScope: Env =
      TwoEnv(trie0,trie1,byItem0,byItem1,
             scope map { case (i,n) => (i,n+1) },
             place)

    // Leave a block scope
    def popScope: Env =
      TwoEnv(trie0,trie1,byItem0,byItem1,
             scope collect { case (i,n) if n>1 => (i,n-1) },
             place)

    // Get typo probabilities for string queries
    def query(typed: String): List[Alt[Item]] =
      typoQuery(trie1,typed)++typoQuery(trie0,typed)

    def exactQuery(typed: String): List[Alt[Item]] = {
      val p = Pr.poissonPDF(typed.length*Pr.typingErrorRate,0)
      (trie1.exact(typed) ++ trie0.exact(typed)) map (Alt(p,_))
    }

    def combinedQuery[A](typed: String, exactProb: Prob, filter: PartialFunction[Item,A], error: String): Scored[A] = {
      val _f = Function.unlift( (x:Alt[Item]) => { if (filter.isDefinedAt(x.x)) Some(Alt(x.p, filter.apply(x.x))) else None } )
      multiples(exactQuery(typed) collect _f map { case Alt(p,t) => Alt(exactProb,t) },
                query(typed) collect _f map { case Alt(p,t) => Alt((1-exactProb)*p,t) },
                error)
    }

    def byItem(t: TypeItem): Scored[Value] = {
      implicit val env: Env = this
      val v0 = byItem1.get(t)
      val v1 = byItem0.get(t)
      val v = if      ((v0 eq null) || v0.isEmpty) v1
              else if ((v1 eq null) || v1.isEmpty) v0
              else v1++v0
      uniformArray(Pr.objectOfItem,v,s"Value of item ${show(t)} not found")
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

  // Look up values by their type
  def objectsOfItem(t: TypeItem)(implicit env: Env): Scored[Value] =
    env.byItem(t)

  // Does a member belong to a type?
  def memberIn(f: Item, t: TypeItem): Boolean = f match {
    case f: Member => f.parent match {
      case p:ClassItem => isSubitem(t,p)
      case _ => false
    }
    case _ => false
  }

  // Assuming a member belongs to a type, what is its fully applied type?
  def typeIn(f: TypeItem, t: Type): Type = f match {
    case f: Member => {
      def p = f.parent
      collectOne(supers(t)){
        case t:ClassType if t.item==p => f.inside.substitute(t.env)
      }.getOrElse(throw new RuntimeException("typeIn didn't find parent"))
    }
    case _ => throw new RuntimeException("typeIn didn't find parent")
  }

  def shadowedInSubType(i: FieldItem, t: ClassItem)(implicit env: Env): Boolean = {
    val c = i.parent
    assert(isSubitem(t,c))
    def loop(t: RefTypeItem): Boolean = t match {
      case _ if c == t => false
      case t:ClassItem => t.declaresField(i.name) || t.superItems.exists(s => isSubitem(s,c) && loop(s))
      case _ => false
    }
    loop(t)
  }

  // What could this be, assuming it's a callable field of the given type?
  def callableFieldScores(t: TypeItem, name: String)(implicit env: Env): Scored[CallableItem] =
    env.combinedQuery(name, Pr.exactCallableField, { case f:CallableItem if memberIn(f,t) => f }, s"Type ${show(t)} has no callable field $name")

  // What could this name be, assuming it is a member of the given type?
  def fieldScores(t: TypeItem, name: String)(implicit env: Env): Scored[Value with Member] =
    env.combinedQuery(name, Pr.exactField, { case f: Value with Member if memberIn(f,t) => f }, s"Type ${show(t)} has no field $name")

  // what could this be, assuming it's a static member of the given type?
  def staticFieldScores(t: TypeItem, name: String)(implicit env: Env): Scored[StaticValue with Member] =
    env.combinedQuery(name, Pr.exactStaticField, { case f:StaticFieldItem if memberIn(f,t) => f case f: EnumConstantItem if memberIn(f,t) => f },
                      s"Type ${show(t)} has no static field $name")

  // What could this be, assuming it is a type field of the given type?
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    env.combinedQuery(name, Pr.exactTypeField, { case f: TypeItem if memberIn(f,t.item) => typeIn(f,t) }, s"Type ${show(t)} has no type field $name")

  // The return type of our ambient function
  def returnType(implicit env: Env): Scored[Type] = {
    def die(scope: String) = fail(s"Can't return from $scope scope")
    env.place.place match {
      case m:MethodItem => single(m.retVal, Pr.certain)
      case c:ConstructorItem => single(VoidType, Pr.certain)
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
