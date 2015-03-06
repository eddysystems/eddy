package tarski

import gnu.trove.TObjectDoubleHashMap
import utility.JavaUtils.poissonPDF
import tarski.AST._
import tarski.Arounds._
import tarski.Items._
import tarski.Scores.{Empty, Scored, Alt, Prob}
import tarski.Types.Type
import tarski.JavaScores.pp
import tarski.Environment._
import utility.Utility

object Pr {

  // Minimum probability before an object is considered a match for a query
  val minimumProbability = .01

  // this many typos are free (after the blanket Pr.typo penalty), exponential decay after that
  @inline def expectedTypos(l: Int): Float = Math.min(l/7.0f, 2.0f)

  // probability reaches .01 at this number of typos
  @inline def maxTypos(l: Int): Float = { val e = expectedTypos(l); Math.max(e*2,e+1) }

  @inline def typoProbability(d: Double, expected: Double, max: Double): Double =
    Math.exp(Math.max(0,d-expected)/max * Math.log(minimumProbability))

  // probability we make d errors when typing l characters
  @inline def typoProbability(d: Double, l: Int): Prob =
    Prob(s"typo d $d, l $l", typoProbability(d, expectedTypos(l), maxTypos(l)))


  /* These errors assume a poisson distribution if typos, which is principled, but a stretch

  // Errors per character
  private val typingErrorRate = .05

  private def normalCDF(mu: Double, sigma: Double, x_in: Double): Double = {
    val x = (x_in-mu)/sigma
    var sum = x
    var value = x
    for (i <- 1 to 20) {
      value = value * x * x / (2*i+1)
      sum += value
    }
    0.5 + (sum/math.sqrt(2*math.Pi))*math.exp(-(x*x)/2.0)
  }

  // return how many mistakes we can make until the probability drops below a threshold
  private def poissonQuantile(lambda: Double, p: Double): Int = {
    var k = math.ceil(lambda).toInt
    while (poissonPDF(lambda,k) > p) k += 1
    k
  }

  @inline def expectedTypos(l: Int) = Pr.typingErrorRate * l
  @inline def maxTypos(l: Int) = {
    Pr.poissonQuantile(expectedTypos(l),Pr.minimumProbability) // Never discards anything because it has too few errors
  }

  def typoProbability(d: Double, l: Int): Prob = {
    val e = l*typingErrorRate
    // probability we make d errors when typing meant.length characters
    Prob(s"typo d $d, l $l",poissonPDF(e,math.ceil(d).toInt))
  }

  */

  def typoProbability(meant: String, typed: String): Prob = {
    val d = JavaTrie.levenshteinDistance(meant.toCharArray, meant.length, typed.toCharArray, typed.length)
    typoProbability(d, typed.length) // could be meant.length, but that's inconsistent with when we don't have meant available
  }

  // Generic likelihood that the user omitted the given qualifier (even though it was necessary)
  def omitQualifier(choice: ClassOrArrayItem): Prob =
    if (Items.inPackage(choice,Base.JavaPkg)) Prob("omit java.*",.8) // stuff in java.lang or java.io (like System.out)
    else Prob("omit other",.3) // TODO: Make this probability higher if there's only one option in values with high likelihood?

  def omitNestedClass(t: TypeItem, c: ClassItem, first: Boolean): Prob =
    if (first) Prob("omit first qualifier for nested class",.8)
    else Prob("omit deep qualifier for nested class",.9)

  def omitPackage(t:TypeItem, p: Items.Package): Prob = {
    if (Items.inPackage(t,Base.JavaLangPkg)) Prob("omit java.lang for type",.9) // if shadowed
    else if (Items.inPackage(p,Base.JavaPkg)) Prob("omit java.* for type",.7)
    else Prob("omit package import",.5)
  }

  val argPosErrorRate = .2

  def swapArgs[A](meant: List[A], typed: List[A]): Prob = {
    def c(f: Float)(a:Seq[A], i:Int, b:Seq[A], j:Int): Float = f
    val d = StringMatching.editDistance(meant, typed, c(1.0f), c(1.0f), c(1.0f), c(.5f))
    val e = meant.length*argPosErrorRate
    // probability we make d errors when typing meant.length arguments
    Prob(s"swap args (meant $meant, typed $typed)",poissonPDF(e,math.ceil(d).toInt))
  }

  // TODO: many of these should be functions of the parts that go into the tree node they represent

  // occam's razor: bias towards smaller things
  val base = Prob("base",.98)

  // denoteLit -- these should be dependent on the contents of the literal
  val intLit = base
  val longIntLit = base // an int literal that's too long for an int
  val longLit = base
  val floatLit = base
  val doubleLit = base
  val charLit = base
  val stringLit = base

  val weirdParens = Prob("parens around type or callable",.8)
  val weirdParensStmt = Prob("parens around stmt",.1)
  val parensInsideParens = Prob("parens inside parens",.7)
  val parens = Prob("parens",1)

  // denoteType(AType)
  // Type[]
  def arrayType(t: Type): Prob = base // given type t, how likely is t[]

  // denoteType(AExp)
  // Exp.Type
  val typeFieldOfExp = Prob("type field of exp",.6)
  val boxType = Prob("box type",.5)

  // Unqualified values that are in or out of scope
  val inScope = Prob("in scope",1)
  val outOfScope = Prob("out of scope",.8)
  private val outOfScopeJavaLangPkg = Prob("out of scope, java.lang package", .8)
  private val outOfScopeSamePkg = Prob("out of scope, other class, same pkg", .7)
  private val outOfScopeJavaPkg = Prob("out of scope, java.* package", .6)
  private val outOfScopeOtherPackage = Prob("out of scope, other package",.1)
  def scope(i: ChildItem)(implicit env: Env): Prob =
    if (env.inScope(i)) Pr.inScope
    else if (inClass(env.place.place,i.parent)) Pr.outOfScope
    else if (pkg(env.place.place) == pkg(i.parent)) Pr.outOfScopeSamePkg
    else if (inPackage(i, Base.JavaLangPkg)) Pr.outOfScopeJavaLangPkg
    else if (inPackage(i, Base.JavaPkg)) Pr.outOfScopeJavaPkg
    else Pr.outOfScopeOtherPackage
  def scope(i: ThisOrSuper)(implicit env: Env): Prob =
    if (env.inScope(i)) Pr.inScope
    else Pr.outOfScope

  // Exp.staticMethod -- an instance object is used for a static method
  val staticFieldCallableWithObject = Prob("static field callable with object",.9)
  // Type.constructor
  val constructorFieldCallable = Prob("constructor field callable",.9)
  // Exp.constructor (for inner classes only)
  val constructorFieldCallableWithObject = Prob("constructor field callable with object",.8)
  val constructorFieldCallableWithoutObject = Prob("constructor field callable without object",.7)
  // Exp.constructor (for non-inner classes)
  val constructorFieldCallableWithSpuriousObject = Prob("static constructor field callable with object",.2)
  // C++-style type arguments for a callable
  val typeApplyCallable = Prob("C++ style type application for callable",.8)
  // new in front of a non-constructor callable
  val dropNew = Prob("drop new",.1)
  val qualifiedStaticNew = Prob("qualified new of static class", .3) // nobody does this
  val qualifiedNewWithUnrelatedObject = Prob("wrongly qualified new", .001) // we completely ignore what you just said. Not likely.
  val qualifiedNew = Prob("qualified new", 1)
  val dropNewQualifier = Prob("dropped new qualifier", .02) // we ignore what you just said, it was unnecessary, and possibly wrong (we didn't check)

  // (<A1>C)<A2> without parentheses
  val badNestedTypeArgs = Prob("bad nested type args",.1)
  // (new C)<A> without parentheses
  val badNewInsideTypeArgs = Prob("new inside type args",.1)

  // denoteExp(AExp)
  val staticFieldExpWithObject = Prob("static field with object",.7)
  val fieldExp = base
  def callExp(list: KList[AExp], around: Around) =
    if (around.isParens && (list.list.size < 2 || list.isInstanceOf[CommaList[_]])) base
    else Prob(s"call (list $list, around $around)",.6)
  def indexCallExp(list: KList[AExp], around: Around) =
    if (around.isBracks && list.list.size == 1) base
    else if (around.isBracks) Prob(s"multiple index call ${list.list.size}",.6)
    else Prob(s"weird index call ${list.list.size} $around",.5)
  val unaryExp = base // should be a function of operator and types
  val trueInstanceofExp = Prob("true instanceof test",.5)
  val falseInstanceofExp = Prob("false instanceof test",.5)
  val discardTypeArgsForInstanceOf = Prob("drop typeargs for instanceof",.9)
  val boxInstanceOf = Prob("box primitive type for instanceof",.1)
  val binaryExpCastZero = Prob("binary cast zero",.5) // Replace x == 0 with x == null or x == false
  val stringCompare = Prob("String == String",.6) // hardly ever what you want to do
  val stringEquals = Prob("String.equals(String)",.9)
  val assignCastZero = Prob("assign 0 to reftype or boolean",.3) // replace x = 0 or return 0 with x = null or x = false
  val castExp = base // should be a function of from/to types
  val condExp = base // should be a function of inside types
  val arrayExp = Prob("array",.8) // {1,2,3}, should be a function of around, types, number of things inside

  // denoteBool(AExp)
  def insertComparison(t: Type): Prob = Prob(s"insert comparison $t",.6) // how likely is it that someone forgot a comparison to obtain a bool (depending on type).

  // denoteIndex
  val insertedCastIndexExp = Prob("inserted cast index exp",.1) // it's unlikely.

  // Completely drop types or type parameters
  def ignoreVarType(edited: Boolean)  = Prob("ignore var type",if (edited) 1e-5 else 1e-3)
  def ignoreTypeArgs(edited: Boolean) = Prob("ignore type args",if (edited) 1e-5 else 1e-3)

  // denoteStmt(AStmt)
  val emptyStmt = Prob("empty stmt",.2) // empty statements are rarely written down
  val holeStmt = base // incomplete statements are common though
  val expStmtsSplit = Prob("exp stmts split",.3) // tried to write a non-expression statement as a statement, had to be split
  val assignmentAsVarStmt = Prob("assign as var stmt",.4) // should depend on types: e.g. explicit constructor calls are more likely
  val blockStmt = base
  val assertStmt = base
  val throwStmt = base // should depend on type of thing thrown
  val syncStmt = base // should depend on the variable used to synchronize
  val ifStmt = base
  val ifElseStmt = base
  val whileStmt = base
  val doStmt = base
  val forStmt = base
  val expForStmt = Prob("exp for stmt",.9)
  val blockForStmt = Prob("block for stmt",.6)
  val forEachArrayNoType = Prob("foreach array no type",.7)
  val ellipsisCatchException = Prob("ellipsis catching Exception", .8)
  val ellipsisCatchThrowable = Prob("ellipsis catching Throwable", .3)
  def catchAround(around: Around) = if (around.isParens) base else Prob("no parens in catch", .5)
  def catchColon(colon: Boolean) = if (colon) Prob("colon type declaration", .7) else base
  val dropPure = Prob("drop pure expression",.5)

  val exact = Prob("exact",1)
  val typo = Prob("typo",.5)
  assert(pp(exact) > pp(typo))
  val objectOfType = base
  val objectOfItem = base
  val globalByItem = Prob("global by item",.6)

  // ArgMatching
  def dropArgs(dropped: Int) = Prob(s"drop $dropped args",math.pow(.3, dropped))
  def shuffleArgs = Prob("shuffle args",.5)
  def addArg = Prob("add arg",.5)
  def missingArgList = Prob("missing argument list",.2)
  val variadicCall = Prob("variadic call",1)
  val arrayContract = Prob("contract into array",.3)
  val contractToObjectArray = Prob("contract to Object[]",.1)
  val convertToArray = Prob("convert to array",.5)
  val specialCall = Prob("special call",.1) // x[f] or (x f) instead of x.f, x f y instead of x.f(y), etc.

  // Named simple values
  val parse = Prob("parse",1)
  val forwardThis = Prob("forward this",1)
  val forwardSuper = Prob("forward super",1)
  val constructor = Prob("constructor",1)
  val notDropNew = Prob("not drop new",1)
  val reasonable = Prob("reasonable",1)
  val ignoreMissingType = Prob("ignore missing type",1)

  // priors are both for objects (found via byItem) and qualifiers (in Env.collect or Env.flatMap).
  // All should be considered to have .* in the end
  val priors: TObjectDoubleHashMap[String] = {
    val m = new TObjectDoubleHashMap[String]()
    m.put("java.lang.System.err",.99)
    m.put("java.lang.StrictMath",.9)
    m
  }
  val anonymousObject = Prob("anonymous prior",1.0)
  def objectPrior(s: String) = { val p = priors.get(s); Prob("prior", if (p == 0.0) 1.0 else p) } // for now, no prior means 1
  def qualifierPrior(i: Item) = {
    // find the biases of all parents and accumulate
    def nest(i: Item): (String,Prob) = if (i.name.isEmpty) ("",anonymousObject) else i match {
      case m:Member => { // for members, go up and re-trace the path through the qualified name
        val n = nest(m.parent)
        val name = n._1 + '.' + m.name
        if (n._1.isEmpty) ("",anonymousObject) else (name, JavaScores.pmul(n._2, objectPrior(name)))
      }
      case rp:RootPackage => (rp.name,objectPrior(rp.name)) // any qualified name ends with a RootPackage
      case _ => ("",anonymousObject) // anonymous things and their descendants, can't have priors for those
    }
    nest(i)._2
  }

  // StringModifiers should return Nil if they don't have anything to say about a name, and never return the name itself
  val modifiedName = Prob("modify name",.6)
  val addGetSet = Prob("add get/set",.9)
  val addIs = Prob("add is",.9)

  private type NameModifier = Array[Char] => List[Alt[Array[Char]]]
  private val modifiers: List[NameModifier] = List(
    // get or set, if the name doesn't already have get or set
    s => { val c = Utility.capitalize(s); List(Alt(addGetSet,Array('g','e','t') ++ c), Alt(addGetSet,Array('s','e','t') ++ c)) },
    s => List(Alt(addIs,Array('i','s') ++ Utility.capitalize(s)))
  )
  def modifyName(s: Array[Char]): Scored[Array[Char]] = JavaScores.listGood(modifiers flatMap (_(s)))
}
