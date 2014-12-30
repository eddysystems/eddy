package tarski

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import ambiguity.JavaUtils._
import ambiguity.Utility._
import tarski.Items._
import tarski.Scores._
import tarski.JavaScores._
import tarski.Tokens._
import tarski.Tries._
import tarski.Types._
import scala.collection.mutable
import scala.annotation.tailrec

object Environment {
  // Minimum probability before an object is considered a match for a query
  val minimumProbability = .01

  // Lookup a string approximately-only (ignoring exact matches)
  private def typoQuery(trie: Trie[Item], typed: Array[Char]): List[Alt[Item]] = {
    val expected = typed.length * Pr.typingErrorRate
    val maxErrors = Pr.poissonQuantile(expected,minimumProbability) // Never discards anything because it has too few errors
    JavaTrie.levenshteinLookup(trie,typed,maxErrors,expected,minimumProbability)
  }

  // Information about where we are
  // TODO: add information about static scope
  // TODO: add information about whether we're in a spot where we can delegate constructor calls
  case class PlaceInfo(place: PlaceItem,
                       breakable: Boolean = false,
                       continuable: Boolean = false,
                       labels: List[String] = Nil) {
    // Can we forward to a constructor of class c?
    // TODO: Restrict to first statement of the constructor
    def forwardThisPossible(c: ClassItem): Boolean = place match {
      case cons:ConstructorItem => c==cons.parent && c.constructors.length>1
      case _ => false
    }
    def forwardSuperPossible(c: ClassItem): Boolean = place match {
      case cons:ConstructorItem => c==cons.parent.base.item && c.constructors.length>0
      case _ => false
    }
    // TODO: in static scope?
  }
  val localPlace = PlaceInfo(Base.LocalPkg)

  // An environment for name resolution
  abstract class Env {
    // Where we are
    def scope: Map[Item,Int]
    def place: PlaceInfo
    def move(to: PlaceInfo): Env

    // Add more objects
    def extend(things: Array[Item], scope: Map[Item,Int]): Env

    // Add local objects (they all appear in inScope with priority scope)
    def extendLocal(things: Array[Item], scope: Int = 1): Env =
      extend(things,(things map ((_,scope))).toMap)

    // Is an item in scope and not shadowed by another item?
    private lazy val _inScope: java.util.Set[Item] = {
      val set = new java.util.HashSet[Item]
      Base.extraEnv.allItems foreach { case t:LangTypeItem => set.add(t); case _ => () }
      val best = new mutable.HashMap[String,(Item,Int)]
      scope foreach { case (i,n) => if (!best.contains(i.name) || n < best(i.name)._2) best(i.name) = (i,n) }
      best foreach { case (_,(i,n)) => set.add(i) }
      set
    }
    @inline final def inScope(i: Item): Boolean = _inScope.contains(i)

    // Enter and leave block scopes
    def pushScope: Env
    def popScope: Env

    // Add variables and fields
    def newVariable(name: String, t: Type, isFinal: Boolean) = place.place match {
      case c: CallableItem =>
        if (scope exists { case (v:LocalVariableItem,_) => v.name == name; case _ => false })
          fail(s"Invalid new local variable $name: already exists.")
        else {
          val x = LocalVariableItem(name,t,isFinal)
          known((extend(Array(x),Map((x,0))),x))
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
          known((extend(Array(x),Map((x,0))),x))
        }
      case _ => fail("Cannot declare fields outside of class or interface declarations.")
    }

    // Get exact and typo probabilities for string queries
    def _exactQuery(typed: Array[Char]): List[Item]
    def _typoQuery(typed: Array[Char]): List[Alt[Item]]
    def _query[A](typed: Array[Char], exactProb: Prob, filter: PartialFunction[Item,A], error: => String): Scored[A] = {
      @tailrec def exact(is: List[Item], s: Scored[A]): Scored[A] = is match {
        case Nil => s
        case i::is => exact(is,if (filter.isDefinedAt(i)) Best(exactProb,filter.apply(i),s) else s)
      }
      @tailrec def approx(is: List[Alt[Item]], as: List[Alt[A]]): List[Alt[A]] = is match {
        case Nil => as
        case Alt(p,i)::is => approx(is,if (filter.isDefinedAt(i)) Alt(p,filter.apply(i))::as else as)
      }
      exact(_exactQuery(typed),biased(pcomp(exactProb),list(approx(_typoQuery(typed),Nil),error)))
    }

    // Convenience aliases taking String
    @inline final def exactQuery(typed: String): List[Item] = _exactQuery(typed.toCharArray)
    @inline final def typoQuery(typed: String): List[Alt[Item]] = _typoQuery(typed.toCharArray)
    @inline final def query[A](typed: String, exactProb: Prob, filter: PartialFunction[Item,A], error: => String): Scored[A] =
      _query(typed.toCharArray,exactProb,filter,error)

    // Lookup by type.item
    def byItem(t: TypeItem): Scored[Value]

    // Fragile or slow, only use for tests
    def exactLocal(name: String): LocalVariableItem
    def allItems: Array[Item]

    // get the innermost (current) ThisItem
    def getThis: ThisItem = scope.collect({ case (i:ThisItem,n) => (i,n) }).minBy(_._2)._1
  }

  // Constructors for Env
  object Env {
    def apply(items: Array[Item], scope: Map[Item,Int] = Map.empty, place: PlaceInfo = localPlace): Env =
        TwoEnv(Trie(items),Trie.empty,
               valuesByItem(items),new java.util.HashMap[TypeItem,Array[Value]](),
               scope,place)
  }

  case class ThreeEnv(private val sTrie: Trie[Item], // never rebuilt (large)
                      private val dTrie: DTrie[Item], // occasionally rebuilt (medium)
                      private val vTrie: Trie[Item], // rebuilt all the time, including by this Env's functions returning new Envs (small)
                      private val sByItem: java.util.Map[TypeItem,Array[Value]],
                      private val dByItem: java.util.Map[TypeItem,Array[Value]],
                      private val vByItem: java.util.Map[TypeItem,Array[Value]],
                      scope: Map[Item,Int], place: PlaceInfo) extends Env {

    val emptyValues = new Array[Value](0)

    override def move(to: PlaceInfo): Env = ThreeEnv(sTrie,dTrie,vTrie,sByItem,dByItem,vByItem,scope,to)

    // Enter and leave block scopes
    override def pushScope: Env = ThreeEnv(sTrie,dTrie,vTrie,sByItem,dByItem,vByItem,
                                           scope map { case (i,n) => (i,n+1) },
                                           place)

    override def popScope: Env = ThreeEnv(sTrie,dTrie,vTrie,sByItem,dByItem,vByItem,
                                           scope collect { case (i,n) if n>1 => (i,n-1) },
                                           place)

    // Lookup by type.item
    override def byItem(t: TypeItem): Scored[Value] = {
      implicit val env: Env = this
      val v0 = sByItem.get(t)
      val v0r = if (v0 == null) emptyValues else v0
      val v1 = dByItem.get(t)
      val v1r = if (v1 == null) emptyValues else v1 filter (!_.deleted)
      val v2 = vByItem.get(t)
      val v2r = if (v2 == null) emptyValues else v2
      uniform(Pr.objectOfItem, v0r ++ v1r ++ v2r, s"Value of item ${show(t)} not found")
    }

    // Add more objects
    override def extend(things: Array[Item], scope: Map[Item, Int]): Env =
      ThreeEnv(sTrie,dTrie,vTrie ++ things, sByItem,dByItem,valuesByItem(vTrie.values++things),this.scope++scope,place)

    // Slow, use only for tests
    override def allItems: Array[Item] = sTrie.values ++ (dTrie.values filter (!_.deleted)) ++ vTrie.values

    // Fragile or slow, only use for tests
    override def exactLocal(name: String): LocalVariableItem = {
      val query = name.toCharArray
      def options(t: Trie[Item]) = t exact query collect { case x: LocalVariableItem => x }
      options(sTrie)++options(dTrie)++options(vTrie) match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }

    override def _exactQuery(typed: Array[Char]): List[Item] =
      sTrie.exact(typed) ++ dTrie.exact(typed) ++ vTrie.exact(typed)

    // Get exact and typo probabilities for string queries
    override def _typoQuery(typed: Array[Char]): List[Alt[Item]] =
      Environment.typoQuery(sTrie,typed)++Environment.typoQuery(dTrie,typed)++Environment.typoQuery(vTrie,typed)
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
             this.scope++scope,place)

    def move(to: PlaceInfo) =
      TwoEnv(trie0,trie1,byItem0,byItem1,scope,to)

    // Fragile, only use for tests
    def exactLocal(name: String): LocalVariableItem = {
      val query = name.toCharArray
      def options(t: Trie[Item]) = t exact query collect { case x: LocalVariableItem => x }
      options(trie0)++options(trie1) match {
        case List(x) => x
        case Nil => throw new RuntimeException(s"No local variable $name")
        case xs => throw new RuntimeException(s"Multiple local variables $name: $xs")
      }
    }

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
    // TODO: should match camel-case smartly (requires word database?)
    def _typoQuery(typed: Array[Char]): List[Alt[Item]] =
      Environment.typoQuery(trie1,typed)++Environment.typoQuery(trie0,typed)

    def _exactQuery(typed: Array[Char]): List[Item] =
      trie1.exact(typed) ++ trie0.exact(typed)

    def byItem(t: TypeItem): Scored[Value] = {
      implicit val env: Env = this
      val v0 = byItem1.get(t)
      val v1 = byItem0.get(t)
      val v = if      ((v0 eq null) || v0.isEmpty) v1
              else if ((v1 eq null) || v1.isEmpty) v0
              else v1++v0
      uniform(Pr.objectOfItem,v,s"Value of item ${show(t)} not found")
    }
  }

  // TODO other things that influence probability:
  // - TODO: kind (e.g. for types: primitive types are more likely, java.lang types are more likely)
  // - TODO: things that are in scope are more likely
  // - TODO: things that are almost in scope are more likely (declared in package from which other symbols are imported)
  // - TODO: things that appear often in this file/class/function are more likely
  // - TODO: things that are declared close by are more likely

  // What could this name be, assuming it is a type?
  def typeScores(name: String)(implicit env: Env): Scored[Type] = {
    env.query(name, Pr.exactType, { case t:TypeItem => t.raw }, s"Type $name not found")
  }

  // What could it be, given it's a callable?
  def callableScores(name: String)(implicit env: Env): Scored[PseudoCallableItem] =
    env.query(name, Pr.exactCallable, {
      case t:CallableItem => t
      case t@ThisItem(c) if env.place.forwardThisPossible(c) => t
      case t@SuperItem(c) if env.place.forwardSuperPossible(c.item) => t
    }, s"Callable $name not found")

  // What could this be, we know it's a value
  def valueScores(name: String)(implicit env: Env): Scored[Value] =
    env.query(name, Pr.exactValue, { case t:Value => t }, s"Value $name not found")

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
  def callableFieldScores(t: TypeItem, name: String, error: => String)(implicit env: Env): Scored[CallableItem] =
    env.query(name, Pr.exactCallableField, { case f:CallableItem if memberIn(f,t) => f }, s"Type ${show(t)} has no callable field $name: $error")

  // What could this name be, assuming it is a member of the given type?
  def fieldScores(t: TypeItem, name: String)(implicit env: Env): Scored[Value with Member] =
    env.query(name, Pr.exactField, { case f: Value with Member if memberIn(f,t) => f }, s"Type ${show(t)} has no field $name")

  // What could this be, assuming it is a type field of the given type?
  def typeFieldScores(t: Type, name: String)(implicit env: Env): Scored[Type] =
    env.query(name, Pr.exactTypeField, { case f: TypeItem if memberIn(f,t.item) => typeIn(f,t) }, s"Type ${show(t)} has no type field $name")

  // The return type of our ambient function
  def returnType(implicit env: Env): Scored[Type] = {
    def die(scope: String) = fail(s"Can't return from $scope scope")
    env.place.place match {
      case m:MethodItem => known(m.retVal)
      case c:ConstructorItem => known(VoidType)
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
