package tarski

import Scores.Prob

object Probabilities {

  // TODO: many of these should be functions of the parts that go into the tree node they represent

  // for transformations that are only technical in nature (for instance filters) and shouldn't affect probabilities
  val passThrough = Prob(1.0)

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
  // "void"
  val voidType = base
  // "int", "byte" etc
  val primType = base
  // Type.Type
  val fieldType = base
  // Type[]
  val arrayType = base

  // denoteType(AExp)
  // (Type)
  val parensAroundType = Prob(.9)
  // Exp.Type
  val typeFieldOfValue = Prob(.6)
  // Type.Type
  val typeFieldOfType = base

  // denoteValue(Value)
  val parameterValue = passThrough
  val localValue = passThrough
  val staticFieldValue = passThrough
  val enumConstantValue = passThrough
  val thisValue = passThrough
  val localFieldValue = passThrough
  val superFieldValue = Prob(.5) // this value is defined in super but shadowed in this
  val shadowedFieldValue = Prob(.5) // this value is defined in some superclass but shadowed somewhere along the way
  val fieldValue = base

  // denoteCallable(AExp)
  val localMethodCallable = passThrough
  val methodCallable = passThrough
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

  // denoteExp(AExp)
  val parenExp = base
  val staticFieldExp = base
  val enumFieldExp = base
  val staticFieldExpWithObject = Prob(.8)
  val enumFieldExpWithObject = Prob(.6) // enum {BLAH} x; x.BLAH ... really?
  val fieldExp = base
  val callExp = base // should be a function of around, list type
  val indexCallExp = base // should be a function of around, list type, number of indices
  val unaryExp = base // should be a function of operator and types
  val binaryExp = base // should be a function of operator and types
  val castExp = base // should be a function of from/to types
  val condExp = base // should be a function of inside types
  val assignExp = base // should be a function of operator and types
  val arrayExp = Prob(.8) // {1,2,3}, should be a function of around, types, number of things inside

  // denoteBool(AExp)
  val boolExp = passThrough

  // denoteNonVoid(AExp)
  val nonVoidExp = passThrough

  // denoteArray(AExp)
  val arrayTypeExp = passThrough

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
  val assignmentAsVarStmt = Prob(.4) // should depend on types: e.g. explicit constructor calls are more likely
  val blockStmt = base
  val assertStmt = base
  val breakStmt = base
  val continueStmt = base
  val returnStmt = base
  val throwStmt = base // should depend on type of thing thrown
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
}
