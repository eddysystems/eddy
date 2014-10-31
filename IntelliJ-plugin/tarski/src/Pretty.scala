package tarski

import AST._
import tarski.Tokens._

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
  val LowestFix    = N // Sentinel
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
  val JuxtListFix  = N // Juxtaposition lists
  val NewFix       = N // new x
  val ApplyFix     = L // x[y], x(y)
  val FieldFix     = L // x.y
  val HighestFix   = N // Parentheses, etc.

  // Pretty printing type class
  type Tokens = List[Token]
  abstract class Pretty[-A] {
    def tokens_(x: A): Tokens
    def fixity_(x: A): Fixity
  }
  def tokens[A](x: A)(implicit p: Pretty[A]) = p.tokens_(x)
  def fixity[A](x: A)(implicit p: Pretty[A]) = p.fixity_(x)
  def prec[A](x: A)(implicit p: Pretty[A]) = fixity(x).prec

  // Pretty printing utilities
  def parens[A](x: A)(implicit p: Pretty[A]) = LParenTok() :: tokens(x) ::: List(RParenTok())
  def typeBracket[A](x: A)(implicit p: Pretty[A]) = LtTok() :: tokens(x) ::: List(GtTok())
  def parensUnless[A](x: A, safe: Boolean)(implicit p: Pretty[A]) = if (safe) tokens(x) else parens(x)
  def toInt(b: Boolean): Int = if (b) 1 else 0
  def left [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensUnless(x, slot.prec-toInt(slot.assoc==2) < prec(x))
  def right[A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensUnless(x, slot.prec-toInt(slot.assoc==4) < prec(x))
  def non  [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensUnless(x, slot.prec < prec(x))

  // AST expressions
  implicit val ExpPretty: Pretty[Exp] = new Pretty[Exp] {
    def fixity_(e: Exp): Fixity = e match {
      case NameExp(_)|LitExp(_)|ParenExp(_)|WildExp(None) => HighestFix
      case FieldExp(_,_,_)|MethodRefExp(_,_,_)|NewRefExp(_,_) => FieldFix
      case ApplyExp(_,_)|TypeApplyExp(_,_)|IndexExp(_,_) => ApplyFix
      case NewExp(_,_) => NewFix
      case WildExp(Some(_)) => WildFix
      case UnaryExp(op,_) => fixity(op)
      case BinaryExp(op,_,_) => fixity(op)
      case CastExp(_,_) => PrefixFix
      case CondExp(_,_,_) => CondFix
      case AssignExp(_,_,_) => AssignFix
    }
    def tokens_(e: Exp): Tokens = e match {
      case NameExp(n) => tokens(n)
      case LitExp(x) => tokens(x)
      case ParenExp(e) => parens(e)
      case FieldExp(e,t,f) => left(FieldFix,e) ::: DotTok() :: tokens(t) ::: tokens(f)
      case MethodRefExp(e,t,f) => left(FieldFix,e) ::: ColonColonTok() :: tokens(t) ::: tokens(f)
      case NewRefExp(e,t) => left(FieldFix,e) ::: ColonColonTok() :: tokens(t) ::: List(NewTok())
      case IndexExp(e,i) => left(ApplyFix,e) ::: LBrackTok() :: tokens(i) ::: List(RBrackTok())
      case TypeApplyExp(e,t) => left(ApplyFix,e) ::: typeBracket(t)
      case ApplyExp(e,a) => left(ApplyFix,e) ::: parens(a) // TODO: f x should stay f x, not turn to f(x)
      case NewExp(t,e) => tokens(t) ::: List(NewTok()) ::: right(NewFix,e)
      case WildExp(None) => List(QuestionTok())
      case WildExp(Some((b,t))) => QuestionTok() :: token(b) :: right(WildFix,t)
      case UnaryExp(op,e) if prefix(op) => tokens(op) ::: right(PrefixFix,e)
      case UnaryExp(op,e)               => left(PostfixFix,e) ::: tokens(op)
      case BinaryExp(op,x,y) => { val s = fixity(op); left(s,x) ::: token(op) :: right(s,y) }
      case CastExp(t,e) => parens(t) ::: right(PrefixFix,e)
      case CondExp(c,t,f) => left(CondFix,c) ::: QuestionTok() :: tokens(t) ::: ColonTok() :: right(CondFix,f)
      case AssignExp(x,op,y) => left(AssignFix,x) ::: token(op) :: right(AssignFix,y)
    }
  }

  // AST types
  implicit val TypePretty: Pretty[Type] = new Pretty[Type] {
    def fixity_(t: Type): Fixity = t match {
      case NameType(_)|WildType(None) => HighestFix
      case ModType(_,_) => ModFix
      case ArrayType(_)|ApplyType(_,_) => ApplyFix
      case FieldType(_,_) => FieldFix
      case WildType(Some(_)) => WildFix
    }
    def tokens_(t: Type): Tokens = t match {
      case NameType(n) => tokens(n)
      case ModType(m,t) => tokens(m) ::: right(ModFix,t)
      case ArrayType(t) => left(ApplyFix,t) ::: List(LBrackTok(),RBrackTok())
      case ApplyType(t,a) => left(ApplyFix,t) ::: typeBracket(a)
      case FieldType(t,f) => left(FieldFix,t) ::: DotTok() :: tokens(f)
      case WildType(None) => List(QuestionTok())
      case WildType(Some((b,t))) => QuestionTok() :: token(b) :: right(WildFix,t)
    }
  }

  // Names
  def tokens(name: Name): Tokens = List(IdentTok(name))

  // Operators
  def prefix(op: UnaryOp): Boolean = op match {
    case PreDecOp()|PreIncOp()|PosOp()|NegOp()|CompOp()|NotOp() => true
    case PostDecOp()|PostIncOp() => false
  }
  def fixity(op: UnaryOp): Fixity = if (prefix(op)) PrefixFix else PostfixFix
  def fixity(op: BinaryOp): Fixity = op match {
    case OrOrOp() => OrOrFix
    case AndAndOp() => AndAndFix
    case OrOp() => OrFix
    case XorOp() => XorFix
    case AndOp() => AndFix
    case EqOp()|NeOp() => EqFix
    case LtOp()|GtOp()|LeOp()|GeOp()|InstanceofOp() => RelFix
    case LShiftOp()|RShiftOp()|UnsignedRShiftOp() => ShiftFix
    case AddOp()|SubOp() => AddFix
    case MulOp()|DivOp()|ModOp() => MulFix
  }
  def tokens(op: UnaryOp): Tokens = List(op match {
    case PreDecOp()|PostDecOp() => MinusMinusTok()
    case PreIncOp()|PostIncOp() => PlusPlusTok()
    case PosOp() => PlusTok()
    case NegOp() => MinusTok()
    case CompOp() => CompTok()
    case NotOp() => NotTok()
  })
  def token(op: BinaryOp): Token = op match {
    case MulOp() => MulTok()
    case DivOp() => DivTok()
    case ModOp() => ModTok()
    case AddOp() => PlusTok()
    case SubOp() => MinusTok()
    case LShiftOp() => LShiftTok()
    case RShiftOp() => RShiftTok()
    case UnsignedRShiftOp() => UnsignedRShiftTok()
    case LtOp() => LtTok()
    case GtOp() => GtTok()
    case LeOp() => LeTok()
    case GeOp() => GeTok()
    case InstanceofOp() => InstanceofTok()
    case EqOp() => EqEqTok()
    case NeOp() => NeTok()
    case AndOp() => AndTok()
    case XorOp() => XorTok()
    case OrOp() => OrTok()
    case AndAndOp() => AndAndTok()
    case OrOrOp() => OrOrTok()
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

  // Literals
  def tokens(x: Lit): Tokens = List(x match {
    case IntLit(v) => IntLitTok(v)
    case LongLit(v) => LongLitTok(v)
    case FloatLit(v) => FloatLitTok(v)
    case DoubleLit(v) => DoubleLitTok(v)
    case BoolLit(b) => BoolLitTok(b)
    case CharLit(v) => CharLitTok(v)
    case StringLit(v) => StringLitTok(v)
    case NullLit() => NullLitTok()
  })

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

  // Lists
  def tokens(t: Option[KList[Type]]): Tokens =
    t map (typeBracket(_)) getOrElse Nil
  def separate(ts: List[Tokens], sep: Tokens): Tokens = ts match {
    case Nil => Nil
    case List(x) => x
    case x :: y => x ::: sep ::: separate(y,sep)
  }
  implicit def KListPretty[A](implicit p: Pretty[A]): Pretty[KList[A]] = new Pretty[KList[A]] {
    def kind(k: KList[A]) = k match {
      case EmptyList() => (LowestFix,Nil)
      case CommaList(_) => (CommaListFix,List(CommaTok()))
      case JuxtList(_) => (JuxtListFix,Nil)
      case AndList(_) => (AndListFix,List(AndTok()))
    }
    def fixity_(k: KList[A]) = kind(k)._1
    def tokens_(k: KList[A]) = {
      val (s,sep) = kind(k)
      separate(k.list.map(non(s,_)),sep)
    }
  }
}
