package tarski

import tarski.AST.CommaList
import tarski.Denotations.{Callable, Exp}
import tarski.Items.{ClassMember, FieldItem, MethodItem, TypeItem}
import tarski.Scores._
import tarski.Types.Type
import tarski.JavaScores._
import ambiguity.JavaUtils.poissonPDF

object Pr {

  val typingErrorRate = .15

  def normalCDF(mu: Double, sigma: Double, x_in: Double): Double = {
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
  def poissonQuantile(lambda: Double, p: Double): Int = {
    var k = math.ceil(lambda).toInt
    while (poissonPDF(lambda,k) > p) k += 1
    k
  }

  def typoProbability(d: Double, l: Int): Prob = {
    val e = l*typingErrorRate
    // probability we make d errors when typing meant.length characters
    Prob(s"typo d $d, l $l",poissonPDF(e,math.ceil(d).toInt))
  }

  def typoProbability(meant: String, typed: String): Prob = {
    val d = JavaTrie.levenshteinDistance(meant.toCharArray, meant.length, typed.toCharArray, typed.length)
    typoProbability(d, typed.length) // could be meant.length, but that's inconsistent with when we don't have meant available
  }

  // generic likelihood that the user omitted a qualifier (even though it was necessary), based on the possible values
  // for the qualifying objects, and the object chosen as qualifier
  def omitQualifier[A <: ClassMember](probs: Scored[Exp], choice: Exp, item: A): Prob = {
    if (probs.isSingle) Prob("omit one choice",.8) // Only choice
    else if (choice.item.qualifiedName.nonEmpty && choice.item.qualifiedName.get.startsWith("java.lang."))
      Prob("omit java.lang",.8) // stuff in java.lang (like System.*)
    else Prob("omit other",.3) // TODO: Make this probability higher if there's only one option in values with high likelihood?
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

  // denoteType(AType)
  // Type.Type
  val fieldType = base // Given Types t and f, how likely is a type expression t.f (can assume t.f is legal)
  // Type[]
  def arrayType(t: Type): Prob = base // given type t, how likely is t[]

  // denoteTypeArg
  val wildcard = base
  val subWildcard = base
  val superWildcard = Prob("super wildcard",.6)

  // denoteType(AExp)
  // (Type)
  val parensAroundType = Prob("parens around type",.8) // given type t, how likely is (t) (maybe more likely for generic types)
  // Type.Type
  val typeFieldOfType = fieldType
  // Exp.Type
  val typeFieldOfExp = pmul(Prob("type field of exp",.6),fieldType)
  // Type<TArg,...>
  val typeApply = base

  // field f is declared in super but shadowed in this, how likely is it the user forgot to qualify?
  def superFieldValue(values: Scored[Exp], c: TypeItem, f: FieldItem) = Prob("super field value",.8)

  // a field requires qualification (with obj), which requires a cast (to c), how likely is it that the user forgot the qualification?
  def shadowedFieldValue(values: Scored[Exp], obj: Exp, c: TypeItem, f: FieldItem): Prob = fieldValue(values, obj, f)

  // a field requires qualification with one of values, how likely is it that the user forgot to qualify with obj?
  def fieldValue(values: Scored[Exp], obj: Exp, f: FieldItem) = omitQualifier(values, obj, f)

  // equivalent of fieldValue, shadowedFieldValue, superFieldValue for methods
  def methodCallable(values: Scored[Exp], obj: Exp, f: MethodItem) = omitQualifier(values, obj, f)
  def shadowedMethodCallable(values: Scored[Exp], obj: Exp, c: TypeItem, f: MethodItem) = methodCallable(values, obj, f)
  def superMethodCallable(values: Scored[Exp], c: TypeItem, f: MethodItem) = Prob("super method callable",.8)

  // (callable)
  val parensAroundCallable = Prob("parens around callable",.8)
  // Type.staticMethod
  val staticFieldCallable = base
  // Exp.staticMethod -- an instance object is used for a static method
  val staticFieldCallableWithObject = Prob("static field callable with object",.9)
  // Exp.method
  val methodFieldCallable = base
  // Type.constructor (not legal in Java. Also makes no sense)
  val constructorFieldCallable = Prob("constructor field callable",.5)
  // Exp.constructor (not legal, and makes even less sense)
  val constructorFieldCallableWithObject = Prob("constructor field callable with object",.4)

  // denoteExp(AExp)
  val parenExp = base
  val staticFieldExp = base
  val enumFieldExp = base
  val staticFieldExpWithObject = Prob("static field with object",.8)
  val enumFieldExpWithObject = Prob("enum field with object",.6) // enum {BLAH} x; x.BLAH ... really?
  val fieldExp = base
  def callExp(list: AST.KList[AST.AExp], around: AST.Around) =
    if (around == AST.ParenAround && (list.list.size < 2 || list.isInstanceOf[CommaList[_]])) base
    else Prob(s"call (list $list, around $around)",.6)
  def indexCallExp(list: AST.KList[AST.AExp], around: AST.Around) =
    if (around == AST.BrackAround && list.list.size == 1) base
    else if (around == AST.BrackAround) Prob(s"multiple index call ${list.list.size}",.6)
    else Prob(s"weird index call ${list.list.size} $around",.5)
  val unaryExp = base // should be a function of operator and types
  val binaryExp = base // should be a function of operator and types
  val castExp = base // should be a function of from/to types
  val condExp = base // should be a function of inside types
  val assignExp = base // should be a function of operator and types
  val arrayExp = Prob("array",.8) // {1,2,3}, should be a function of around, types, number of things inside

  // denoteBool(AExp)
  def insertComparison(t: Type): Prob = Prob(s"insert comparison $t",.6) // how likely is it that someone forgot a comparison to obtain a bool (depending on type).

  // denoteIndex
  val insertedCastIndexExp = Prob("inserted cast index exp",.1) // it's unlikely.

  // denoteStmt(AStmt)
  val emptyStmt = Prob("empty stmt",.2) // empty statements are rarely written down
  val holeStmt = base // incomplete statements are common though
  val varInitNone = base
  val varInit = base
  var varDecl = base
  val varStmt = base // should depend on matchiness of declaration and initializer types
  val expStmt = base // could also be 1, nothing happens here
  val expStmtsSplit = Prob("exp stmts split",.3) // tried to write a non-expression statement as a statement, had to be split
  val assignmentAsVarStmt = Prob("assign as var stmt",.4) // should depend on types: e.g. explicit constructor calls are more likely
  val blockStmt = base
  val assertStmt = base
  val breakStmt = base
  val continueStmt = base
  val returnStmt = base
  val throwStmt = base // should depend on type of thing thrown
  val syncStmt = base // should depend on the variable used to synchronize
  val ifStmt = base
  val ifElseStmt = base
  val whileStmt = base
  val doStmt = base
  val forStmt = base
  val expForStmt = Prob("exp for stmt",.9)
  val blockForStmt = Prob("block for stmt",.6)
  val forEachArray = Prob("foreach array",.9)
  val forEachArrayNoType = Prob("foreach array no type",.7)

  // denoteLabel
  val labelNone = base

  val exactType = base
  val exactCallable = base
  val exactValue = base
  val objectOfType = base
  val exactValueOfType = base
  val objectOfItem = base
  val exactCallableField = base
  val exactField = base
  val exactStaticField = base
  val exactTypeField = base

  // ArgMatching
  def dropArgs(dropped: Int) = Prob(s"drop $dropped args",math.pow(.3, dropped))
  def shuffleArgs = Prob("shuffle args",.5)
  def addArg = Prob("add arg",.5)

  // Named simple values
  val parse = Prob("parse",1)
  val forwardThis = Prob("forward this",1)
  val forwardSuper = Prob("forward super",1)
}
