package tarski

import AST._
import tarski.Items.NamedItem
import tarski.Tokens._
import tarski.Denotations._
import scala.language.implicitConversions
import ambiguity.Utility._

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
    override def toString = s"Fixity($prec,$assoc)"
  }

  // The fixity declarations below go from lowest to highest precedence
  private var nextPrec = 0
  private def nextFixity(assoc: Int) = {
    val p = nextPrec
    nextPrec += 1
    new Fixity(8*p+assoc)
  }
  private def N  = nextFixity(0) // nonassociative, good
  private def NB = nextFixity(1) // nonassociative, bad
  private def L  = nextFixity(2) // left, good
  private def LB = nextFixity(3) // left, bad
  private def R  = nextFixity(4) // right, good
  private def RB = nextFixity(5) // right, bad

  // All the kinds of expression fixity in Java, from lowest to highest precedence.
  val SemiFix      = N // Separating statements
  val LowestExpFix = N // Sentinel
  val ModFix       = R // Modifiers
  val CommaListFix = N // ,-delimited lists
  val AndListFix   = N // &-delimited lists
  val LambdaFix    = R
  val AssignFix    = R
  val CondFix      = R // 15.25
  val OrOrFix      = L // 15.24
  val AndAndFix    = L // 15.23
  val OrFix        = L // 15.22
  val XorFix       = L
  val AndFix       = L
  val EqFix        = LB // 15.21: Equality operators
  val RelFix       = LB // 15.20: Relational operators
  val ShiftFix     = L // 15.19
  val AddFix       = L // 15.18
  val MulFix       = L // 15.17
  val PrefixFix    = N // 15.15.  Includes casts.  The worries about (p)+q shouldn't apply for pretty printing purposes.
  val PostfixFix   = N // 15.14
  val WildFix      = N // ? extends T (? alone is HighestFix)
  val JuxtFix      = N // Juxtaposition lists
  val NewFix       = N // new x
  val ApplyFix     = L // x[y], x(y)
  val FieldFix     = L // x.y
  val HighestFix   = N // Parentheses, etc.

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
    val (s,sep) = k match {
      case EmptyList => (LowestExpFix,Nil)
      case SingleList(x) => pretty(x)
      case CommaList(_) => (CommaListFix,List(CommaTok()))
      case JuxtList(_) => (JuxtFix,Nil)
      case AndList(_) => (AndListFix,List(AndTok()))
    }
    (s,separate(k.list.map(non(s,_)),sep))
  }

  // Names
  def tokens(name: Name): Tokens = List(IdentTok(name))

  // AST types
  implicit def prettyType(t: Type): (Fixity,Tokens) = t match {
    case NameType(n) => prettyName(n)
    case ModType(m,t) => fix(ModFix, tokens(m) ::: right(_,t))
    case ArrayType(t) => fix(ApplyFix, left(_,t) ::: List(LBrackTok(),RBrackTok()))
    case ApplyType(t,a) => fix(ApplyFix, left(_,t) ::: typeBracket(a))
    case FieldType(t,f) => fix(FieldFix, left(_,t) ::: DotTok() :: tokens(f))
    case WildType(None) => (HighestFix, List(QuestionTok()))
    case WildType(Some((b,t))) => fix(WildFix, QuestionTok() :: token(b) :: right(_,t))
  }
  implicit def prettyTypeArgs(t: Option[KList[Type]]): (Fixity,Tokens) =
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
  implicit def prettyExp(e: Exp): (Fixity,Tokens) = e match {
    case NameExp(n) => pretty(n)
    case LitExp(x) => prettyLit(x)
    case ParenExp(e) => (HighestFix,parens(e))
    case FieldExp(e,t,f) => fix(FieldFix, left(_,e) ::: DotTok() :: tokens(t) ::: tokens(f))
    case MethodRefExp(e,t,f) => fix(FieldFix, left(_,e) ::: ColonColonTok() :: tokens(t) ::: tokens(f))
    case NewRefExp(e,t) => fix(FieldFix, left(_,e) ::: ColonColonTok() :: tokens(t) ::: List(NewTok()))
    case TypeApplyExp(e,t) => fix(ApplyFix, left(_,e) ::: typeBracket(t))
    case NewExp(t,e) => fix(NewFix, tokens(t) ::: List(NewTok()) ::: right(_,e))
    case WildExp(None) => (HighestFix, List(QuestionTok()))
    case WildExp(Some((b,t))) => fix(WildFix, QuestionTok() :: token(b) :: right(_,t))
    case UnaryExp(op,e) if isPrefix(op) => fix(PrefixFix, token(op) :: right(_,e))
    case UnaryExp(op,e)               => fix(PostfixFix, left(_,e) ::: List(token(op)))
    case BinaryExp(op,x,y) => { val (s,t) = pretty(op); (s, left(s,x) ::: t ::: right(s,y)) }
    case CastExp(t,e) => (PrefixFix, parens(t) ::: right(PrefixFix,e))
    case CondExp(c,t,f) => fix(CondFix, s => left(s,c) ::: QuestionTok() :: tokens(t) ::: ColonTok() :: right(s,f))
    case AssignExp(op,x,y) => fix(AssignFix, s => left(s,x) ::: token(op) :: right(s,y))
    case ArrayExp(xs,a) => around(xs,a)
    case ApplyExp(e,xs,a) => {
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
  implicit def prettyStmt(s: Stmt): (Fixity,Tokens) = {
    def key[A](key: () => Token, x: Option[A])(implicit p: Pretty[A]): (Fixity,Tokens) =
      (SemiFix, key() :: (x map (tokens(_)) getOrElse Nil) ::: List(SemiTok()))
    s match {
      case EmptyStmt() => (SemiFix,List(SemiTok()))
      case VarStmt(m,t,v) => (SemiFix, m.map(tokens).flatten ::: tokens(t) ::: tokens(v))
      case BlockStmt(b) => (HighestFix, LCurlyTok() :: tokens(b) ::: List(RCurlyTok()))
      case ExpStmt(e) => (SemiFix, tokens(e) ::: List(SemiTok()))
      case AssertStmt(c,m) => notImplemented
      case BreakStmt(l)    => key(BreakTok,l)
      case ContinueStmt(l) => key(ContinueTok,l)
      case ReturnStmt(e)   => key(ReturnTok,e)
      case ThrowStmt(e)    => key(ThrowTok,Some(e))
      case SyncStmt(e,b) => notImplemented
    }
  }
  implicit def prettyStmts(ss: List[Stmt]): (Fixity,Tokens) = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyVar(d: (NameDims,Option[Exp])): (Fixity,Tokens) = d match {
    case (x,None) => pretty(x)
    case (x,Some(e)) => fix(AssignFix, tokens(x) ::: EqTok() :: right(_,e))
  }
  implicit def prettyNameDims(n: NameDims): (Fixity,Tokens) = n match {
    case (x,0) => pretty(x)
    case (x,n) => fix(ApplyFix, left(_,(x,n-1)) ::: List(LBrackTok(),RBrackTok()))
    }

  // Literals
  implicit def prettyLit(x: Lit) = (HighestFix, List(x match {
    case AST.IntLit(v) => IntLitTok(v)
    case AST.LongLit(v) => LongLitTok(v)
    case AST.FloatLit(v) => FloatLitTok(v)
    case AST.DoubleLit(v) => DoubleLitTok(v)
    case AST.BoolLit(b) => BoolLitTok(b)
    case AST.CharLit(v) => CharLitTok(v)
    case AST.StringLit(v) => StringLitTok(v)
    case AST.NullLit() => NullLitTok()
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
  implicit def prettyNamedItem(i: NamedItem): (Fixity,Tokens) = {
    throw new NotImplementedError("Need to think about scoping.  This routine should return FieldFix sometimes.")
    prettyName(i.name)
  }
  implicit def prettyExpDen(e: ExpDen): (Fixity,Tokens) = e match {
    case ParameterExpDen(x) => pretty(x)
    case LocalVariableExpDen(x) => pretty(x)
    case EnumConstantExpDen(x) => pretty(x)
    case CastExpDen(t,x) => fix(PrefixFix, parens(t) ::: right(_,x))
    case UnaryExpDen(op,x) if isPrefix(op) => fix(PrefixFix, token(op) :: right(_,x))
    case UnaryExpDen(op,x)               => fix(PostfixFix, left(_,x) ::: List(token(op)))
    case BinaryExpDen(op,x,y) => { val (s,t) = pretty(op); (s, left(s,x) ::: t ::: right(s,y)) }
    case AssignExpDen(op,x,y) => fix(AssignFix, s => left(s,x) ::: token(op) :: right(s,y))
    case ParenExpDen(x) => (HighestFix,parens(x))
    case ApplyExpDen(f,a) => (ApplyFix, (f match {
      case MethodDen(x,f) => left(FieldFix,x) ::: DotTok() :: tokens(f.name)
      case LocalMethodDen(f) => tokens(f)
      case StaticMethodDen(f) => tokens(f)
      case NewDen(c) => NewTok() :: tokens(c.containing)
      case ForwardDen(c) => List(ThisTok())
    }) ::: LParenTok() :: separate(a.map(tokens(_)),List(CommaTok())) ::: List(RParenTok()))
    case FieldExpDen(x,f) => fix(FieldFix, left(_,x) ::: tokens(f.name))
    case LocalFieldExpDen(f) => pretty(f)
    case StaticFieldExpDen(f) => pretty(f)
    case IndexExpDen(e,i) => fix(ApplyFix, left(_,e) ::: LBrackTok() :: tokens(i) ::: List(RBrackTok()))
  }
}
