package tarski

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import ambiguity.Utility._
import ambiguity.Locations._
import tarski.JavaItems._
import tarski.Items._
import tarski.Scores._
import tarski.Tokens._
import tarski.Tries._
import tarski.Types._
import scala.collection.mutable
import scala.annotation.tailrec

object Environment {
  // Turn on to skip all approximate lookups
  val exactOnly = false

  // Information about where we are
  // TODO: add information about static scope
  // TODO: add information about whether we're in a spot where we can delegate constructor calls
  case class PlaceInfo(place: ParentItem,
                       breakable: Boolean = false,
                       continuable: Boolean = false,
                       lastEdit: SLoc = SLoc.unknown) {
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

    def lastEditIn(r: SRange): Boolean = r contains lastEdit
  }
  val localPlace = PlaceInfo(LocalPkg)

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
      val best = new mutable.HashMap[String,(Int,List[Item])]
      scope foreach { case (i,n) =>
        val (m,is) = best.getOrElse(i.name,(n,Nil))
        if (n <= m) best(i.name) = (n,i :: (if (n==m) is else Nil))
      }
      best foreach { case (_,(n,is)) => is foreach set.add }
      set
    }
    @inline final def inScope(i: Item): Boolean = _inScope.contains(i)

    // for tests
    def allLocalItems: Array[Item]

    // Enter and leave block scopes
    def pushScope: Env
    def popScope: Env

    // Add variables and fields.  Each variable is consumed by one of the fs.
    def newVariables[A](names: List[String], isFinal: Boolean, fs: List[(Env,Local) => A]): Scored[List[Type] => (Env,List[A])] = {
      val set = names.toSet
      if (set.size < names.size) fail(s"Duplicate name among ${names mkString ", "}")
      else place.place match {
        case c:CallableItem => scope collect { case (v:Local,_) if set contains v.name => v.name } match {
          case Nil => known((ts: List[Type]) => {
            @tailrec def loop(as: List[A], env: Env, ns: List[String], ts: List[Type], fs: List[(Env,Local) => A]): (Env,List[A]) = (ns,ts,fs) match {
              case (Nil,Nil,Nil) => (env,as.reverse)
              case (n::ns,t::ts,f::fs) =>
                val x = Local(n,t,isFinal)
                val e = env.extend(Array(x),Map(x->0))
                loop(f(env,x)::as,e,ns,ts,fs)
              case _ => impossible
            }
            loop(Nil,this,names,ts,fs)
          })
          case bad => fail(s"Invalid new local variables ${bad mkString ", "}: names already exist")
        }
        case _ => fail("Cannot declare local variables outside of methods or constructors.")
      }
    }

    def newVariable(name: String, isFinal: Boolean): Scored[Type => (Env,Local)] =
      newVariables(List(name),isFinal,List((e:Env,x:Local) => x)) map (f => (t: Type) => f(List(t)) match {
        case (e,List(x)) => (e,x)
        case _ => impossible
      })

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
    def _collect[A](typed: Array[Char], error: => String, filter: PartialFunction[Item,A]): Scored[A] = {
      @tailrec def exact(is: List[Item], s: Scored[A]): Scored[A] = is match {
        case Nil => s
        case i::is => exact(is,if (filter.isDefinedAt(i)) bestThen(Pr.exact,filter.apply(i),s) else s)
      }
      @tailrec def approx(is: List[Alt[Item]], as: List[Alt[A]]): List[Alt[A]] = is match {
        case Nil => as
        case Alt(p,i)::is => approx(is,if (filter.isDefinedAt(i)) Alt(p,filter.apply(i))::as else as)
      }
      exact(_exactQuery(typed),if (exactOnly) fail(error)
                               else biased(Pr.typo,list(approx(_typoQuery(typed),Nil),error)))
    }
    def _flatMap[A](typed: Array[Char], error: => String, f: Item => Scored[A]): Scored[A] = {
      @tailrec def exact(is: List[Item], s: Scored[Item]): Scored[Item] = is match {
        case Nil => s
        case i::is => exact(is,bestThen(Pr.exact,i,s))
      }
      @tailrec def approx(is: List[Alt[Item]], as: List[Alt[Item]]): List[Alt[Item]] = is match {
        case Nil => as
        case Alt(p,i)::is => approx(is,Alt(p,i)::as)
      }
      exact(_exactQuery(typed),if (exactOnly) fail(error)
                               else biased(Pr.typo,list(approx(_typoQuery(typed),Nil),error))) flatMap f
    }

    // Convenience aliases taking String
    @inline final def exactQuery(typed: String): List[Item] = _exactQuery(typed.toCharArray)
    @inline final def typoQuery(typed: String): List[Alt[Item]] = _typoQuery(typed.toCharArray)
    @inline final def collect[A](typed: String, error: => String, filter: PartialFunction[Item,A]): Scored[A] =
      _collect(typed.toCharArray,error,filter)
    @inline final def flatMap[A](typed: String, error: => String, f: Item => Scored[A]): Scored[A] =
      _flatMap(typed.toCharArray,error,f)

    // Lookup by type.item
    def byItem(t: TypeItem): Scored[Value]

    // Fragile or slow, only use for tests
    def exactLocal(name: String): Local

    // get the innermost (current) ThisItem
    def getThis: ThisItem = scope.collect({ case (i:ThisItem,n) => (i,n) }).minBy(_._2)._1
  }

  // Constructors for Env 
  object Env {
    def apply(items: Array[Item], scope: Map[Item,Int] = Map.empty, place: PlaceInfo = localPlace): TwoEnv =
        TwoEnv(Trie(items),Trie.empty,
               valuesByItem(items),new java.util.HashMap[TypeItem,Array[Value]](),
               scope,place)
  }

  case class ThreeEnv(private val sTrie: LazyTrie[Item], // creates global (library) items as they are queried
                      private val dTrie: DTrie[Item], // occasionally rebuilt when the project has changed a lot
                      private val vTrie: Trie[Item], // rebuilt all the time, including by this Env's functions returning new Envs (small)
                      private val dByItem: java.util.Map[TypeItem,Array[Value]],
                      private val vByItem: java.util.Map[TypeItem,Array[Value]],
                      scope: Map[Item,Int], place: PlaceInfo) extends Env {

    val emptyValues = new Array[Value](0)

    override def move(to: PlaceInfo): Env = ThreeEnv(sTrie,dTrie,vTrie,dByItem,vByItem,scope,to)

    // Enter and leave block scopes
    override def pushScope: Env = ThreeEnv(sTrie,dTrie,vTrie,dByItem,vByItem,
                                           scope map { case (i,n) => (i,n+1) },
                                           place)

    override def popScope: Env = ThreeEnv(sTrie,dTrie,vTrie,dByItem,vByItem,
                                           scope collect { case (i,n) if n>1 => (i,n-1) },
                                           place)

    // Lookup by type.item
    override def byItem(t: TypeItem): Scored[Value] = {
      implicit val env: Env = this
      val v1 = dByItem.get(t)
      val v1r = if (v1 == null) emptyValues else v1 filter (!_.deleted)
      val v2 = vByItem.get(t)
      val v2r = if (v2 == null) emptyValues else v2
      uniform(Pr.objectOfItem, v1r ++ v2r, s"Value of item ${show(t)} not found")
    }

    // Add more objects
    override def extend(things: Array[Item], scope: Map[Item, Int]): Env =
      ThreeEnv(sTrie,dTrie,vTrie ++ things,dByItem,valuesByItem(vTrie.values++things),this.scope++scope,place)

    // Slow, use only for tests
    def allLocalItems: Array[Item] = (dTrie.values filter (!_.deleted)) ++ vTrie.values

    // Fragile or slow, only use for tests
    override def exactLocal(name: String): Local = {
      val query = name.toCharArray
      def options(t: Queriable[Item]) = t exact query collect { case x:Local => x }
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
      sTrie.typoQuery(typed)++dTrie.typoQuery(typed)++vTrie.typoQuery(typed)
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
    def allLocalItems: Array[Item] = trie1.values

    // Add some new things to an existing environment
    def extend(things: Array[Item], scope: Map[Item,Int]) =
      TwoEnv(trie0,trie1++things,
             byItem0,valuesByItem(trie1.values++things),
             this.scope++scope,place)

    def move(to: PlaceInfo) =
      TwoEnv(trie0,trie1,byItem0,byItem1,scope,to)

    // Fragile, only use for tests
    def exactLocal(name: String): Local = {
      val query = name.toCharArray
      def options(t: Trie[Item]) = t exact query collect { case x: Local => x }
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
      trie1.typoQuery(typed)++trie0.typoQuery(typed)

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

  // Look up values by their type
  def objectsOfItem(t: TypeItem)(implicit env: Env): Scored[Value] =
    env.byItem(t)

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

  // The return type of our ambient function
  def returnType(implicit env: Env): Scored[Type] = {
    def die(scope: String) = fail(s"Can't return from $scope scope")
    env.place.place match {
      case m:MethodItem => known(m.retVal)
      case c:ConstructorItem => known(VoidType)
      case _:Package => die("package")
      case _:ClassItem => die("class or interface")
      case _:UnknownContainerItemBase => die("non-Java item")
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
