package tarski

import AST._
import tarski.Environment.Env
import tarski.Items._
import tarski.Types._
import tarski.Tokens._
import tarski.Denotations._
import scala.language.implicitConversions
import ambiguity.Utility._
import scala.collection.mutable

object Pretty {
  // Fixity and precedence: to parenthesize or not to parenthesize
  //
  // The theory is as follows: we have a set of slots S and expression types T.  Slots include
  //   Three slots (x,y,z) in CondExp(x,y,z)
  //   For each op, two slots (x,y) in BinaryExp(op,x,y)
  //   For each op, one slot x in UnaryOp(x)
  // Expression types include
  //   CondExp
  //   For each op, BinaryExp(op,_,_)
  //   For each op, UnaryExp(op,_)
  // We have a predicate safe(s,t) which is true if type t can safely fit into slot s (without parentheses).
  //
  // In nearly every case, slots and types can be *almost* identified, except for issues of left vs. right
  // associativity.  There is one issue where UnaryExp(PosOp,UnaryExp(PosOp,e)) produces a token stream + + e
  // that can't be concatenated, but that's an issue for tokens to string, not tree to tokens.

  // Fixity includes both precedence and right/left/non-associativity
  class Fixity(private val x: Int) extends AnyVal {
    // x = 8*prec + assoc, where assoc is one of
    //   0: not associative
    //   1: not associative, but should be parenthesized
    //   2: good left associative, a+b+c = (a+b)+c
    //   3:  bad left associative, same but should be parenthesized
    //   4: good right associative, a+b+c = a+(b+c)
    //   5:  bad right associative
    def prec = x & ~7
    def assoc = x & 7
    override def toString = fixityNames(this)+"Fix"
  }
  private val fixityNames = mutable.Map[Fixity,String]()

  // The fixity declarations below go from lowest to highest precedence
  private var nextPrec = 0
  private def nextFixity(assoc: Int)(name: String) = {
    val p = nextPrec
    nextPrec += 1
    val f = new Fixity(8*p+assoc)
    fixityNames(f) = name
    f
  }
  private def N  = nextFixity(0)(_) // nonassociative, good
  private def NB = nextFixity(1)(_) // nonassociative, bad
  private def L  = nextFixity(2)(_) // left, good
  private def LB = nextFixity(3)(_) // left, bad
  private def R  = nextFixity(4)(_) // right, good
  private def RB = nextFixity(5)(_) // right, bad

  // All the kinds of expression fixity in Java, from lowest to highest precedence.
  val SemiFix      = N("Semi") // Separating statements
  val LowestExpFix = N("LowestExp") // Sentinel
  val ModFix       = R("Mod") // Modifiers
  val CommaListFix = N("CommaList") // ,-delimited lists
  val AndListFix   = N("AndList") // &-delimited lists
  val LambdaFix    = R("Lambda")
  val AssignFix    = R("Assign")
  val CondFix      = R("Cond") // 15.25
  val OrOrFix      = L("OrOr") // 15.24
  val AndAndFix    = L("AndAnd") // 15.23
  val OrFix        = L("Or") // 15.22
  val XorFix       = L("Xor")
  val AndFix       = L("And")
  val EqFix        = LB("Eq") // 15.21: Equality operators
  val RelFix       = LB("Rel") // 15.20: Relational operators
  val ShiftFix     = L("Shift") // 15.19
  val AddFix       = L("And") // 15.18
  val MulFix       = L("Mul") // 15.17
  val PrefixFix    = N("Prefix") // 15.15.  Includes casts.  The worries about (p)+q shouldn't apply for pretty printing purposes.
  val PostfixFix   = N("Postfix") // 15.14
  val WildFix      = N("Wild") // ? extends T (? alone is HighestFix)
  val JuxtFix      = N("Juxt") // Juxtaposition lists
  val NewFix       = N("New") // new x
  val ApplyFix     = L("Apply") // x[y], x(y)
  val FieldFix     = L("Field") // x.y
  val HighestFix   = N("Highest") // Parentheses, etc.

  // Top level pretty printing
  type Tokens = List[Token]
  type Pretty[-A] = A => (Fixity,Tokens)
  def pretty[A](x: A)(implicit p: Pretty[A]): (Fixity,Tokens) = p(x)
  def tokens[A](x: A)(implicit p: Pretty[A]): Tokens = p(x)._2

  // Pretty printing utilities
  def parens[A](x: A)(implicit p: Pretty[A]) = LParenTok() :: tokens(x) ::: List(RParenTok())
  def typeBracket[A](x: A)(implicit p: Pretty[A]) = LtTok() :: tokens(x) ::: List(GtTok())
  def parensIf[A](x: A, prec: Int)(implicit p: Pretty[A]) = {
    val (f,ts) = p(x)
    if (prec < f.prec) ts else LParenTok() :: ts ::: List(RParenTok())
  }
  def toInt(b: Boolean): Int = if (b) 1 else 0
  def left [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec-toInt(slot.assoc==2))
  def right[A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec-toInt(slot.assoc==4))
  def non  [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec)
  private def fix[A](s: Fixity, f: Fixity => A) = (s,f(s))

  // Names
  implicit def prettyName(n: Name) = (HighestFix,List(IdentTok(n)))

  // Lists
  def separate(ts: List[Tokens], sep: Tokens): Tokens = ts match {
    case Nil => Nil
    case List(x) => x
    case x :: y => x ::: sep ::: separate(y,sep)
  }
  implicit def prettyList[A](k: KList[A])(implicit p: Pretty[A]): (Fixity,Tokens) = {
    def wrap(s: Fixity, sep: Tokens) = (s,separate(k.list.map(non(s,_)),sep))
    k match {
      case EmptyList => (LowestExpFix,Nil)
      case SingleList(x) => pretty(x)
      case CommaList(_) => wrap(CommaListFix,List(CommaTok()))
      case JuxtList(_) => wrap(JuxtFix,Nil)
      case AndList(_) => wrap(AndListFix,List(AndTok()))
    }
  }

  // Names
  def tokens(name: Name): Tokens = List(IdentTok(name))

  // AST types
  implicit def prettyAType(t: AType): (Fixity,Tokens) = t match {
    case NameAType(n) => prettyName(n)
    case ModAType(m,t) => fix(ModFix, tokens(m) ::: right(_,t))
    case ArrayAType(t) => fix(ApplyFix, left(_,t) ::: List(LBrackTok(),RBrackTok()))
    case ApplyAType(t,a) => fix(ApplyFix, left(_,t) ::: typeBracket(a))
    case FieldAType(t,f) => fix(FieldFix, left(_,t) ::: DotTok() :: tokens(f))
    case WildAType(None) => (HighestFix, List(QuestionTok()))
    case WildAType(Some((b,t))) => fix(WildFix, QuestionTok() :: token(b) :: right(_,t))
  }
  implicit def prettyATypeArgs(t: Option[KList[AType]]): (Fixity,Tokens) =
    (HighestFix, t map (typeBracket(_)) getOrElse Nil)

  // Operators
  def isPrefix(op: UnaryOp): Boolean = op match {
    case PreDecOp()|PreIncOp()|PosOp()|NegOp()|CompOp()|NotOp() => true
    case PostDecOp()|PostIncOp() => false
  }
  def token(op: UnaryOp): Token = op match {
    case PreDecOp()|PostDecOp() => MinusMinusTok()
    case PreIncOp()|PostIncOp() => PlusPlusTok()
    case PosOp() => PlusTok()
    case NegOp() => MinusTok()
    case CompOp() => CompTok()
    case NotOp() => NotTok()
  }
  implicit def prettyBinary(op: BinaryOp): (Fixity,Tokens) = {
    def f(s: Fixity, t: () => Token) = (s,List(t()))
    op match {
      case MulOp() => f(MulFix,MulTok)
      case DivOp() => f(MulFix,DivTok)
      case ModOp() => f(MulFix,ModTok)
      case AddOp() => f(AddFix,PlusTok)
      case SubOp() => f(AddFix,MinusTok)
      case LShiftOp() => f(ShiftFix,LShiftTok)
      case RShiftOp() => f(ShiftFix,RShiftTok)
      case UnsignedRShiftOp() => f(ShiftFix,UnsignedRShiftTok)
      case LtOp() => f(RelFix,LtTok)
      case GtOp() => f(RelFix,GtTok)
      case LeOp() => f(RelFix,LeTok)
      case GeOp() => f(RelFix,GeTok)
      case InstanceofOp() => f(RelFix,InstanceofTok)
      case EqOp() => f(EqFix,EqEqTok)
      case NeOp() => f(EqFix,NeTok)
      case AndOp() => f(AndFix,AndTok)
      case XorOp() => f(XorFix,XorTok)
      case OrOp() => f(OrFix,OrTok)
      case AndAndOp() => f(AndAndFix,AndAndTok)
      case OrOrOp() => f(OrOrFix,OrOrTok)
    }
  }
  def token(op: Option[AssignOp]): Token = op match {
    case None => EqTok()
    case Some(op) => op match {
      case MulOp() => MulEqTok()
      case DivOp() => DivEqTok()
      case ModOp() => ModEqTok()
      case AddOp() => PlusEqTok()
      case SubOp() => MinusEqTok()
      case LShiftOp() => LShiftEqTok()
      case RShiftOp() => RShiftEqTok()
      case UnsignedRShiftOp() => UnsignedRShiftEqTok()
      case AndOp() => AndEqTok()
      case XorOp() => XorEqTok()
      case OrOp() => OrEqTok()
  }}

  // AST expressions
  implicit def prettyAExp(e: AExp): (Fixity,Tokens) = e match {
    case NameAExp(n) => pretty(n)
    case x: ALit => prettyALit(x)
    case ParenAExp(e) => (HighestFix,parens(e))
    case FieldAExp(e,t,f) => fix(FieldFix, left(_,e) ::: DotTok() :: tokens(t) ::: tokens(f))
    case MethodRefAExp(e,t,f) => fix(FieldFix, left(_,e) ::: ColonColonTok() :: tokens(t) ::: tokens(f))
    case NewRefAExp(e,t) => fix(FieldFix, left(_,e) ::: ColonColonTok() :: tokens(t) ::: List(NewTok()))
    case TypeApplyAExp(e,t) => fix(ApplyFix, left(_,e) ::: typeBracket(t))
    case NewAExp(t,e) => fix(NewFix, tokens(t) ::: List(NewTok()) ::: right(_,e))
    case WildAExp(None) => (HighestFix, List(QuestionTok()))
    case WildAExp(Some((b,t))) => fix(WildFix, QuestionTok() :: token(b) :: right(_,t))
    case UnaryAExp(op,e) if isPrefix(op) => fix(PrefixFix, token(op) :: right(_,e))
    case UnaryAExp(op,e)               => fix(PostfixFix, left(_,e) ::: List(token(op)))
    case BinaryAExp(op,x,y) => { val (s,t) = pretty(op); (s, left(s,x) ::: t ::: right(s,y)) }
    case CastAExp(t,e) => (PrefixFix, parens(t) ::: right(PrefixFix,e))
    case CondAExp(c,t,f) => fix(CondFix, s => left(s,c) ::: QuestionTok() :: tokens(t) ::: ColonTok() :: right(s,f))
    case AssignAExp(op,x,y) => fix(AssignFix, s => left(s,x) ::: token(op) :: right(s,y))
    case ArrayAExp(xs,a) => around(xs,a)
    case ApplyAExp(e,xs,a) => {
      val s = a match { case NoAround => JuxtFix; case _ => ApplyFix }
      fix(s, left(_,e) ::: around(xs,a)._2)
    }
  }
  def around[A](xs: KList[A], a: Around)(implicit p: Pretty[A]): (Fixity,Tokens) = (a,xs) match {
    case (NoAround,EmptyList) => throw new RuntimeException("nothing around empty should never happen")
    case (NoAround,SingleList(_)) => throw new RuntimeException("nothing around single should never happen")
    case (NoAround,xs) => pretty(xs)
    case (ParenAround,xs) => (HighestFix, LParenTok() :: tokens(xs) ::: List(RParenTok()))
    case (BrackAround,xs) => (HighestFix, LBrackTok() :: tokens(xs) ::: List(RBrackTok()))
    case (CurlyAround,xs) => (HighestFix, LCurlyTok() :: tokens(xs) ::: List(RCurlyTok()))
  }

  // AST statements
  implicit def prettyAStmt(s: AStmt): (Fixity,Tokens) = {
    def key[A](key: () => Token, x: Option[A])(implicit p: Pretty[A]): (Fixity,Tokens) =
      (SemiFix, key() :: (x map (tokens(_)) getOrElse Nil) ::: List(SemiTok()))
    s match {
      case EmptyAStmt() => (SemiFix,List(SemiTok()))
      case VarAStmt(m,t,v) => (SemiFix, m.map(tokens).flatten ::: tokens(t) ::: tokens(v))
      case BlockAStmt(b) => (HighestFix, LCurlyTok() :: tokens(b) ::: List(RCurlyTok()))
      case ExpAStmt(e) => (SemiFix, tokens(e) ::: List(SemiTok()))
      case AssertAStmt(c,m) => notImplemented
      case BreakAStmt(l)    => key(BreakTok,l)
      case ContinueAStmt(l) => key(ContinueTok,l)
      case ReturnAStmt(e)   => key(ReturnTok,e)
      case ThrowAStmt(e)    => key(ThrowTok,Some(e))
      case SyncAStmt(e,b) => notImplemented
    }
  }
  implicit def prettyAStmts(ss: List[AStmt]): (Fixity,Tokens) = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyAVar(d: (NameDims,Option[AExp])): (Fixity,Tokens) = d match {
    case (x,None) => pretty(x)
    case (x,Some(e)) => fix(AssignFix, tokens(x) ::: EqTok() :: right(_,e))
  }
  implicit def prettyNameDims(n: NameDims): (Fixity,Tokens) = n match {
    case (x,0) => pretty(x)
    case (x,n) => fix(ApplyFix, left(_,(x,n-1)) ::: List(LBrackTok(),RBrackTok()))
    }

  // Literals
  implicit def prettyALit(x: ALit) = (HighestFix, List(x match {
    case IntALit(v) => IntLitTok(v)
    case LongALit(v) => LongLitTok(v)
    case FloatALit(v) => FloatLitTok(v)
    case DoubleALit(v) => DoubleLitTok(v)
    case BoolALit(b) => BoolLitTok(b)
    case CharALit(v) => CharLitTok(v)
    case StringALit(v) => StringLitTok(v)
    case NullALit() => NullLitTok()
  }))

  // Keywords
  def token(b: Bound) = b match {
    case Extends() => ExtendsTok()
    case Super() => SuperTok()
  }
  def tokens(m: Mod): Tokens = m match {
    case Annotation(n) => AtTok() :: tokens(n)
    case Abstract() => List(AbstractTok())
    case Public() => List(PublicTok())
    case Protected() => List(ProtectedTok())
    case Private() => List(PrivateTok())
    case Static() => List(StaticTok())
    case Final() => List(FinalTok())
    case Strictfp() => List(StrictfpTok())
    case Transient() => List(TransientTok())
    case Volatile() => List(VolatileTok())
    case Synchronized() => List(SynchronizedTok())
  }

  // Denotations
  implicit def prettyNamedItem(i: NamedItem)(implicit env: Env): (Fixity,Tokens) = {
    def staticRelativeName(i: NamedItem with Member) =
      if (env.itemInScope(i)) pretty(i.name) else (FieldFix, tokens(i.container) ::: DotTok() :: tokens(i.name))

    i match {
      // types
      case i: TypeItem => staticRelativeName(i)

      // static members
      case i: EnumConstantItem => staticRelativeName(i)
      case i: StaticFieldItem => staticRelativeName(i)
      case i: StaticMethodItem => staticRelativeName(i)

      // non-static things ought to be fully resolved by the expression they're contained in (otherwise the denotation was wrong)
      case i: NamedItem => pretty(i.name)
    }
  }

  implicit def prettyType(t: Type)(implicit env: Env): (Fixity,Tokens) = {
    implicit def hi(t: () => Token) = (HighestFix,List(t()))
    def gen(d: NamedItem, ts: List[Type]) = (ApplyFix, tokens(d) ::: LtTok() :: tokens(CommaList(ts)) ::: List(GtTok()))
    t match {
      // Primitive types
      case VoidType    => VoidTok
      case BooleanType => BooleanTok
      case ByteType    => ByteTok
      case ShortType   => ShortTok
      case IntType     => IntTok
      case LongType    => LongTok
      case FloatType   => FloatTok
      case DoubleType  => DoubleTok
      case CharType    => CharTok
      // Reference types
      case NullType => pretty("nulltype")
      case ObjectType => pretty("Object")
      case ErrorType(n) => pretty(n)
      case SimpleInterfaceType(d) => pretty(d)
      case SimpleClassType(d) => pretty(d)
      case GenericInterfaceType(d,ts) => gen(d,ts)
      case GenericClassType(d,ts) => gen(d,ts)
      case ParamType(x) => pretty(x)
      case IntersectType(ts) => pretty(AndList(ts.toList))
      case ArrayType(t) => (ApplyFix, tokens(t) ::: List(LBrackTok(),RBrackTok()))
    }
  }
  implicit def prettyExp(e: Exp)(implicit env: Env): (Fixity,Tokens) = e match {
    case ParameterExp(x) => pretty(x)
    case LocalVariableExp(x) => pretty(x)
    case EnumConstantExp(x) => pretty(x)
    case ThisExp(i) => if (env.itemInScope(i)) (HighestFix,List(ThisTok())) else (FieldFix, tokens(i.ourItem) ::: DotTok() :: List(ThisTok()))
    case SuperExp(i) => if (env.itemInScope(i)) (HighestFix,List(SuperTok())) else (FieldFix, tokens(i.ourItem) ::: DotTok() :: List(SuperTok()))
    case CastExp(t,x) => fix(PrefixFix, parens(t) ::: right(_,x))
    case UnaryExp(op,x) if isPrefix(op) => fix(PrefixFix, token(op) :: right(_,x))
    case UnaryExp(op,x)               => fix(PostfixFix, left(_,x) ::: List(token(op)))
    case BinaryExp(op,x,y) => { val (s,t) = pretty(op); (s, left(s,x) ::: t ::: right(s,y)) }
    case AssignExp(op,x,y) => fix(AssignFix, s => left(s,x) ::: token(op) :: right(s,y))
    case ParenExp(x) => (HighestFix,parens(x))
    case ApplyExp(f,a) => (ApplyFix, (f match {
      case MethodDen(x,f) => left(FieldFix,x) ::: DotTok() :: tokens(f.name)
      case LocalMethodDen(f) => tokens(f)
      case StaticMethodDen(f) => tokens(f)
      case NewDen(c) => NewTok() :: tokens(c.container)
      case ForwardDen(c) => List(ThisTok())
    }) ::: LParenTok() :: separate(a.map(tokens(_)),List(CommaTok())) ::: List(RParenTok()))
    case FieldExp(x,f) => fix(FieldFix, left(_,x) ::: tokens(f.name))
    case LocalFieldExp(f) => pretty(f)
    case StaticFieldExp(f) => pretty(f)
    case IndexExp(e,i) => fix(ApplyFix, left(_,e) ::: LBrackTok() :: tokens(i) ::: List(RBrackTok()))
    case ArrayExp(t,xs) => (ApplyFix, NewTok() :: tokens(ArrayType(t)) ::: prettyArrayExp(xs)._2)
    case EmptyArrayExp(t,is) => {
      def inner(t: Type, ts: Tokens): Tokens = t match {
        case ArrayType(t) => inner(t, ts ::: List(LBrackTok(),RBrackTok()))
        case t => tokens(t) ::: ts
      }
      def outer(t: Type, is: List[Exp], ts: Tokens): Tokens = (t,is) match {
        case (t,Nil) => inner(t,ts)
        case (ArrayType(t),i::is) => outer(t, is, ts ::: LBrackTok() :: tokens(i) ::: List(RBrackTok()))
        case _ => throw new RuntimeException("type mismatch (not enough array dimensions)")
      }
      (ApplyFix, NewTok() :: outer(t,is,Nil))
    }
  }
  def prettyInit(e: Exp)(implicit env: Env): (Fixity,Tokens) = e match {
    case ArrayExp(_,xs) => prettyArrayExp(xs)
    case e => prettyExp(e)
  }
  def prettyArrayExp(xs: List[Exp])(implicit env: Env): (Fixity,Tokens) =
    (HighestFix, LCurlyTok() :: tokens(CommaList(xs))(prettyList(_)(prettyInit)) ::: List(RCurlyTok()))
  implicit def prettyStmt(s: Stmt)(implicit env: Env): (Fixity,Tokens) = s match {
    case EmptyStmt() => (SemiFix, List(SemiTok()))
    case VarStmt(t,vs) => (SemiFix, tokens(t) ::: tokens(CommaList(vs)) ::: List(SemiTok()))
    case ExpStmt(e) => (SemiFix, tokens(e) ::: List(SemiTok()))
    case BlockStmt(b) => (HighestFix, LCurlyTok() :: tokens(b) ::: List(RCurlyTok()))
  }
  implicit def prettyStmts(ss: List[Stmt])(implicit env: Env): (Fixity,Tokens) = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyVar(v: (LocalVariableItem,Option[Exp]))(implicit env: Env): (Fixity,Tokens) = v match {
    case (x,None) => pretty(x)
    case (x,Some(i)) => fix(AssignFix, tokens(x) ::: EqTok() :: right(_,i)(prettyInit))
  }
  implicit def prettyCallable(c: Callable)(implicit env: Env): (Fixity,Tokens) = c match {
    case MethodDen(x,f) => fix(FieldFix,left(_,x) ::: DotTok() :: tokens(f))
    case LocalMethodDen(f) => pretty(f)
    case StaticMethodDen(f) => pretty(f)
    case NewDen(f) => (ApplyFix,NewTok() :: tokens(f))
    case ForwardDen(f) => (HighestFix,List(ThisTok()))
  }
  implicit def prettyDen(d: Den)(implicit env: Env): (Fixity,Tokens) = d match {
    case TypeDen(t) => prettyType(t)
    case c: Callable => prettyCallable(c)
    case s: Stmt => prettyStmt(s)
    case e: Exp => prettyExp(e)
  }
}
