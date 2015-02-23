package tarski

import com.intellij.psi.PsiElement
import utility.Interrupts
import utility.Utility._
import utility.Locations._
import tarski.JavaItems._
import tarski.Items._
import tarski.Scores._
import tarski.Tokens._
import tarski.Tries._
import tarski.Types._
import tarski.Pretty._
import scala.collection.mutable
import scala.annotation.tailrec

object Environment {
  // Turn on to skip all approximate lookups
  val exactOnly = false

  // Implicit pretty printing
  private implicit val showFlags = abbrevShowFlags
  private implicit val showRange = SRange.unknown

  // Information about where we are
  case class PlaceInfo(place: ParentItem, exactPlace: PsiElement = null,
                       breakable: Boolean = false,
                       continuable: Boolean = false,
                       lastEdit: SLoc = SLoc.unknown) extends RefEq {
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

    // Are we inside a constructor of the given class?
    def insideConstructorOf(cls: ClassOrArrayItem): Boolean = place match {
      case cons:ConstructorItem => cons.parent == cls
      case _ => false
    }

    // Are we inside the given class in a nonstatic context?
    def inClassNonstatic(c: ClassItem): Boolean = {
      @tailrec def loop(x: ParentItem): Boolean = x==c || (x match {
        case x:Member => !x.isStatic && loop(x.parent)
        case _ => false
      })
      loop(place)
    }

    def lastEditIn(r: SRange): Boolean = r contains lastEdit
  }
  val localPlace = PlaceInfo(LocalPkg)
  def PlaceInfoJava(place: ParentItem, exactPlace: PsiElement,
                    breakable: Boolean, continuable: Boolean, lastEdit: Int): PlaceInfo =
    PlaceInfo(place,exactPlace,breakable,continuable,SLoc(lastEdit))

  // An environment for name resolution
  abstract class Env extends Pretty.Scope {
    // Where we are
    def scope: Map[Item,Int]
    def place: PlaceInfo
    def move(to: PlaceInfo): Env

    // A new environment with one more item in it
    def add(item: Item, scope: Int): Env

    // Only for tests!
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

    // Enter a block scope
    def pushScope: Env

    // Add variables and fields.  Each variable is consumed by one of the fs.
    def newVariables[A](names: List[String], isFinal: Boolean, fs: List[(Env,Local) => A]): Scored[List[Type] => (Env,List[A])] = {
      val set = names.toSet
      if (set.size < names.size) fail(s"Duplicate name among ${names mkString ", "}")
      else place.place match {
        case c:CallableItem => scope collect { case (v:Local,_) if set.contains(v.name) => v.name } match {
          case Nil => known((ts: List[Type]) => {
            @tailrec def loop(as: List[A], env: Env, ns: List[String], ts: List[Type], fs: List[(Env,Local) => A]): (Env,List[A]) = (ns,ts,fs) match {
              case (Nil,Nil,Nil) => (env,as.reverse)
              case (n::ns,t::ts,f::fs) =>
                val x = NormalLocal(n,t,isFinal)
                val e = env.add(x,0)
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
          known((add(x,0),x))
        }
      case _ => fail("Cannot declare fields outside of class or interface declarations.")
    }

    // Get exact and typo probabilities for string queries
    protected def _exactQuery(typed: Array[Char]): List[Item]
    protected def _typoQuery(typed: Array[Char]): Scored[Item]
    protected def _collect[A](typed: Array[Char], error: => String, filter: PartialFunction[Item,A]): Scored[A] = {
      @tailrec def exact(is: List[Item], s: Scored[A]): Scored[A] = is match {
        case Nil => s
        case i::is => exact(is,if (filter.isDefinedAt(i)) bestThen(Pr.exact,filter.apply(i),s) else s)
      }
      exact(_exactQuery(typed),if (exactOnly) fail(error)
                               else biased(Pr.typo,_typoQuery(typed).collect(filter,error))) // FIXME
    }
    protected def _flatMap[A](typed: Array[Char], error: => String, f: Item => Scored[A]): Scored[A] = {
      @tailrec def exact(is: List[Item], s: Scored[Item]): Scored[Item] = is match {
        case Nil => s
        case i::is => exact(is,bestThen(Pr.exact,i,s))
      }
      exact(_exactQuery(typed),if (exactOnly) fail(error)
                               else biased(Pr.typo,orError(_typoQuery(typed),error))) flatMap f // FIXME
    }

    protected def _byItem(t: TypeItem): Scored[Value]

    // Convenience aliases taking String (only used in tests)
    @inline final def exactQuery(typed: String): List[Item] = _exactQuery(typed.toCharArray)
    @inline final def typoQuery(typed: String): Scored[Item] = _typoQuery(typed.toCharArray)

    @inline final def collect[A](typed: String, error: => String, filter: PartialFunction[Item,A]): Scored[A] = {
      _collect(typed.toCharArray,error,filter)
    }
    @inline final def flatMap[A](typed: String, error: => String, filter: Item => Scored[A]): Scored[A] = {
      _flatMap(typed.toCharArray,error,filter)
    }

    // Lookup by type.item
    def byItem(t: TypeItem): Scored[Value] = {
      _byItem(t) flatMap { i => single(i,Pr.objectPrior(i.qualified)) }
    }

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

  case class LazyEnv(private val trie0: Queriable[Item], // creates items as they are queried
                     private val trie1: Queriable[Item], // contains only the base items
                     private val added: QueriableItemList, // changed by this environment's extend functions (better be tiny)
                     private val byItem: ValueByItemQuery, // the JavaEnvironment has functions to compute this lazily, we just have to filter the result
                     scope: Map[Item,Int], place: PlaceInfo) extends Env {

    override def toString: String = "Env()"

    val emptyValues = new Array[Value](0)

    override def move(to: PlaceInfo): Env = LazyEnv(trie0,trie1,added,byItem,scope,to)

    // Enter a block scope
    override def pushScope: Env = LazyEnv(trie0,trie1,added,byItem,scope map { case (i,n) => (i,n+1) },place)

    // Lookup by type.item
    protected override def _byItem(t: TypeItem): Scored[Value] = {
      implicit val env: Env = this
      val values = (byItem.query(t) ++ added.query(t)) filter (_.accessible(place))
      uniform(Pr.objectOfItem, values, s"No value of type item ${show(t)} found")
    }

    // return a new environment with one more item in it (doesn't recompute tries)
    override def add(item: Item, scope: Int): Env =
      LazyEnv(trie0,trie1,added.add(item),byItem,this.scope+(item->scope),place)

    override def extend(things: Array[Item], scope: Map[Item, Int]): Env = {
      LazyEnv(trie0,trie1,added.add(things),byItem,this.scope++scope,place)
    }

    protected override def _exactQuery(typed: Array[Char]): List[Item] = {
      if (Interrupts.pending != 0) Interrupts.checkInterrupts()
      (trie1.exact(typed) ++ trie0.exact(typed) ++ added.exact(typed)) filter (_.accessible(place))
    }

    // Get exact and typo probabilities for string queries
    protected override def _typoQuery(typed: Array[Char]): Scored[Item] = {
      if (Interrupts.pending != 0) Interrupts.checkInterrupts()
      (trie1.typoQuery(typed) ++ trie0.typoQuery(typed) ++ added.typoQuery(typed)).filter({
        case i:Item => i.accessible(place)
      }, s"Nothing accessible for ${typed.mkString}")
    }
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
    def add(item: Item, scope: Int) = extend(Array(item), Map(item->scope))
    def extend(things: Array[Item], scope: Map[Item,Int]) =
      TwoEnv(trie0,trie1++things,
             byItem0,valuesByItem(trie1.values++things),
             this.scope++scope,place)

    def move(to: PlaceInfo) =
      TwoEnv(trie0,trie1,byItem0,byItem1,scope,to)

    // Enter a new block scope
    def pushScope: Env =
      TwoEnv(trie0,trie1,byItem0,byItem1,
             scope map { case (i,n) => (i,n+1) },
             place)

    // Get typo probabilities for string queries
    // TODO: should match camel-case smartly (requires word database?)
    protected override def _typoQuery(typed: Array[Char]): Scored[Item] =
      (trie1.typoQuery(typed)++trie0.typoQuery(typed))
        .filter(_.accessible(place),s"Nothing accessible for ${typed.mkString}")

    protected override def _exactQuery(typed: Array[Char]): List[Item] =
      (trie1.exact(typed) ++ trie0.exact(typed)) filter (_.accessible(place))

    protected override def _byItem(t: TypeItem): Scored[Value] = {
      implicit val env: Env = this
      val v0 = byItem1.get(t)
      val v1 = byItem0.get(t)
      val v = if      ((v0 eq null) || v0.isEmpty) v1
              else if ((v1 eq null) || v1.isEmpty) v0
              else v1++v0
      uniform(Pr.objectOfItem,v filter (_.accessible(place)),s"Value of item ${show(t)} not found")
    }

    // Used only for tests
    override def equals(x: Any): Boolean = x.isInstanceOf[TwoEnv]
    override def toString = "TwoEnv(...)"
  }

  // TODO other things that influence probability:
  // - TODO: kind (e.g. for types: primitive types are more likely, java.lang types are more likely)
  // - TODO: things that are in scope are more likely
  // - TODO: things that are almost in scope are more likely (declared in package from which other symbols are imported)
  // - TODO: things that appear often in this file/class/function are more likely
  // - TODO: things that are declared close by are more likely

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
      case _:ClassOrArrayItem => die("class or interface or array")
      case _:UnknownContainerItemBase => die("non-Java item")
    }
  }
}
