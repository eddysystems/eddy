package tarski

import tarski.AST.CommaList
import tarski.Scores.{Scored, Prob}
import tarski.Denotations.{Callable, Exp, typeOf}
import tarski.Items._
import tarski.Types.Type

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

  def factorial(k: Int): Float = {
    var f = 1.0f
    for (i <- 2 to k)
      f *= i
    f
  }

  def poissonPDF(lambda: Double, k: Int): Double = {
    math.pow(lambda, k) / factorial(k) * math.exp(-lambda)
  }

  // return how many mistakes we can make until the probability drops below a threshold
  def poissonQuantile(lambda: Double, p: Prob): Int = {
    var k = math.ceil(lambda).toInt
    while (poissonPDF(lambda,k) > p) k += 1
    k
  }

  def typoProbability(d: Double, l: Int): Prob = {
    val e = l*typingErrorRate
    // probability we make d errors when typing meant.length characters
    Prob(poissonPDF(e,math.ceil(d).toInt))
  }

  def typoProbability(meant: String, typed: String): Prob = {
    val d = StringMatching.levenshteinDistance(meant, typed)
    typoProbability(d, typed.length) // could be meant.length, but that's inconsistent with when we don't have meant available
  }

  // generic likelihood that the user omitted a qualifier (even though it was necessary), based on the possible values
  // for the qualifying objects, and the object chosen as qualifier
  def omitQualifier[A <: ClassMember](probs: Scored[Exp], choice: Exp, item: A): Prob = {
    if (probs.isSingle) Prob(.8) // One choice
    else Prob(.3) // TODO: make this probability higher if there's only one option in values with high likelihood?
  }

  val argPosErrorRate = .2

  def swapArgs[A](meant: List[A], typed: List[A]): Prob = {
    def c(f: Float)(a:Seq[A], i:Int, b:Seq[A], j:Int): Float = f
    val d = StringMatching.editDistance(meant, typed, c(1.0f), c(1.0f), c(1.0f), c(.5f))
    val e = meant.length*argPosErrorRate
    // probability we make d errors when typing meant.length arguments
    Prob(poissonPDF(e,math.ceil(d).toInt))
  }

  // TODO: many of these should be functions of the parts that go into the tree node they represent

  val certain = Prob(1.0)
  val never = Prob(0.0)

  // for transformations that are only technical in nature (for instance filters) and shouldn't affect probabilities
  val passThrough = certain

  // occam's razor: bias towards smaller things
  val base = Prob(.98)

  // denoteLit -- these should be dependent on the contents of the literal
  val intLit = base
  val longIntLit = base // an int literal that's too long for an int
  val longLit = base
  val floatLit = base
  val doubleLit = base
  val charLit = base
  val booleanLit = base
  val nullLit = base
  val stringLit = base

  // denoteType(AType)
  // Type.Type
  def fieldType(t: Type, f: Type): Prob = base // given Types t and f, how likely is a type expression t.f (can assume t.f is legal)
  // Type[]
  def arrayType(t: Type): Prob = base // given type t, how likely is t[]

  // denoteType(AExp)
  // (Type)
  def parensAroundType(t: Type): Prob = Prob(.8) // given type t, how likely is (t) (maybe more likely for generic types)
  // Type.Type
  def typeFieldOfType(t: Type, f: Type): Prob = fieldType(t,f)
  // Exp.Type
  def typeFieldOfExp(t: Exp, f: Type): Prob = Prob(.6) * fieldType(typeOf(t),f)

  // denoteValue(Value)
  val parameterValue = passThrough
  val localValue = passThrough
  val staticFieldValue = passThrough
  val enumConstantValue = passThrough
  val thisValue = passThrough
  val localFieldValue = passThrough

  // field f is declared in super but shadowed in this, how likely is it the user forgot to qualify?
  def superFieldValue(values: Scored[Exp], c: TypeItem, f: FieldItem) = Prob(.8)

  // a field requires qualification (with obj), which requires a cast (to c), how likely is it that the user forgot the qualification?
  def shadowedFieldValue(values: Scored[Exp], obj: Exp, c: TypeItem, f: FieldItem): Prob = fieldValue(values, obj, f)

  // a field requires qualification with one of values, how likely is it that the user forgot to qualify with obj?
  def fieldValue(values: Scored[Exp], obj: Exp, f: FieldItem) = omitQualifier(values, obj, f)

  // denoteCallable(AExp)
  val localMethodCallable = passThrough

  // equivalent of fieldValue, shadowedFieldValue, superFieldValue for methods
  def methodCallable(values: Scored[Exp], obj: Exp, f: SMethodItem) = omitQualifier(values, obj, f)
  def shadowedMethodCallable(values: Scored[Exp], obj: Exp, c: TypeItem, f: SMethodItem) = methodCallable(values, obj, f)
  def superMethodCallable(values: Scored[Exp], c: TypeItem, f: SMethodItem) = Prob(.8)

  val staticMethodCallable = passThrough
  val constructorCallable = passThrough
  // (callable)
  val parensAroundCallable = Prob(.8)
  // Type.staticMethod
  val staticFieldCallable = base
  // Exp.staticMethod -- an instance object is used for a static method
  val staticFieldCallableWithObject = Prob(.9)
  // Exp.method
  val methodFieldCallable = base
  // Type.constructor
  val constructorFieldCallable = Prob(.8)
  // Exp.constructor
  val constructorFieldCallableWithObject = Prob(.6)

  // denoteExp(AExp)
  val parenExp = base
  val staticFieldExp = base
  val enumFieldExp = base
  val staticFieldExpWithObject = Prob(.8)
  val enumFieldExpWithObject = Prob(.6) // enum {BLAH} x; x.BLAH ... really?
  val fieldExp = base
  def permuteArgs(f: Callable, typedargs: List[Exp], permutedargs: List[Exp]) = swapArgs(permutedargs, typedargs)
  def callExp(list: AST.KList[AST.AExp], around: AST.Around) = if (around == AST.ParenAround && (list.list.size < 2 || list.isInstanceOf[CommaList[AST.AExp]])) base else Prob(.8)
  def indexCallExp(list: AST.KList[AST.AExp], around: AST.Around) = if (around == AST.BrackAround && list.list.size == 1) base else Prob(.8)
  val unaryExp = base // should be a function of operator and types
  val binaryExp = base // should be a function of operator and types
  val castExp = base // should be a function of from/to types
  val condExp = base // should be a function of inside types
  val assignExp = base // should be a function of operator and types
  val arrayExp = Prob(.8) // {1,2,3}, should be a function of around, types, number of things inside

  // denoteBool(AExp)
  val boolExp = passThrough
  def insertComparison(t: Type): Prob = Prob(.6) // how likely is it that someone forgot a comparison to obtain a bool (depending on type).

  // denoteNonVoid(AExp)
  val nonVoidExp = passThrough

  // denoteArray(AExp)
  val arrayTypeExp = passThrough

  // denoteRef
  val refExp = passThrough

  // denoteVariable(AExp)
  val variableExp = passThrough

  // denoteStmt(AStmt)
  val emptyStmt = Prob(.2) // empty statements are rarely written down
  val holeStmt = base // incomplete statements are common though
  val varInitNone = base
  val varInit = base
  var varDeclNil = passThrough
  var varDecl = base
  val varStmt = base // should depend on matchiness of declaration and initializer types
  val expStmt = base // could also be passThrough, nothing happens here
  val expStmtsSplit = Prob(.3) // tried to write a non-expression statement as a statement, had to be split
  val assignmentAsVarStmt = Prob(.4) // should depend on types: e.g. explicit constructor calls are more likely
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
  val expForStmt = Prob(.9)
  val blockForStmt = Prob(.6)
  val forEachArray = Prob(.9)
  val forEachArrayNoType = Prob(.7)

  // denoteLabel
  val labelNone = base

  // denoteStmts
  val stmtList = passThrough

  // Environment
  val newVariable = certain
  val newField = certain
  val newStaticField = certain

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
}
