package tarski

import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.Env
import tarski.Items._
import tarski.Operators._
import tarski.Tokens._
import tarski.Types._

import ambiguity.Utility.impossible

import scala.collection.mutable
import scala.language.implicitConversions

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
  def parens[A](x: A)(implicit p: Pretty[A]): Tokens = parens(tokens(x))
  def parens(ts: Tokens): Tokens = LParenTok :: ts ::: List(RParenTok)
  def around[A](x: A, a: Around)(implicit p: Pretty[A]): (Fixity,Tokens) = {
    val t = pretty(x)
    def left (g: Group) = g match { case Paren => LParenTok; case Brack => LBrackTok; case Curly => LCurlyTok }
    def right(g: Group) = g match { case Paren => RParenTok; case Brack => RBrackTok; case Curly => RCurlyTok }
    a match {
      case NoAround => t
      case Grouped(l,r) => (HighestFix,left(l) :: t._2 ::: List(right(r)))
    }
  }
  def typeBracket[A](x: A)(implicit p: Pretty[A]) = LtTok :: tokens(x) ::: List(GtTok)
  def parensIf[A](x: A, prec: Int)(implicit p: Pretty[A]) = {
    val (f,ts) = p(x)
    if (prec < f.prec) ts else LParenTok :: ts ::: List(RParenTok)
  }
  def toInt(b: Boolean): Int = if (b) 1 else 0
  def left [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec-toInt(slot.assoc==2))
  def right[A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec-toInt(slot.assoc==4))
  def non  [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec)
  private def fix[A](s: Fixity, f: Fixity => A) = (s,f(s))

  // Names
  implicit def prettyName(n: Name) = (HighestFix,List(IdentTok(n)))
  def prettyDims[A](x: A, n: Dims)(implicit p: Pretty[A]): (Fixity,Tokens) = {
    if (n == 0) pretty(x)
    else fix(ApplyFix, left(_,x) ::: List.fill(n)(List(LBrackTok,RBrackTok)).flatten)
  }

  // Options
  implicit def prettyOption[A](x: Option[A])(implicit p: Pretty[A]): (Fixity,Tokens) = x match {
    case None => Nil
    case Some(x) => p(x)
  }

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
      case CommaList(_) => wrap(CommaListFix,List(CommaTok))
      case JuxtList(_) => wrap(JuxtFix,Nil)
      case AndList(_) => wrap(AndListFix,List(AndTok))
    }
  }

  // Names
  def tokens(name: Name): Tokens = List(IdentTok(name))

  // Operators
  def isPrefix(op: UnaryOp): Boolean = op match {
    case PreDecOp|PreIncOp|PosOp|NegOp|CompOp|NotOp => true
    case PostDecOp|PostIncOp => false
  }
  def token(op: UnaryOp): Token = op match {
    case PreDecOp|PostDecOp => MinusMinusTok
    case PreIncOp|PostIncOp => PlusPlusTok
    case PosOp => PlusTok
    case NegOp => MinusTok
    case CompOp => CompTok
    case NotOp => NotTok
  }
  implicit def prettyBinary(op: BinaryOp): (Fixity,Tokens) = {
    def f(s: Fixity, t: Token) = (s,List(t))
    op match {
      case MulOp => f(MulFix,MulTok)
      case DivOp => f(MulFix,DivTok)
      case ModOp => f(MulFix,ModTok)
      case AddOp => f(AddFix,PlusTok)
      case SubOp => f(AddFix,MinusTok)
      case LShiftOp => f(ShiftFix,LShiftTok)
      case RShiftOp => f(ShiftFix,RShiftTok)
      case UnsignedRShiftOp => f(ShiftFix,UnsignedRShiftTok)
      case LtOp => f(RelFix,LtTok)
      case GtOp => f(RelFix,GtTok)
      case LeOp => f(RelFix,LeTok)
      case GeOp => f(RelFix,GeTok)
      case EqOp => f(EqFix,EqEqTok)
      case NeOp => f(EqFix,NeTok)
      case AndOp => f(AndFix,AndTok)
      case XorOp => f(XorFix,XorTok)
      case OrOp => f(OrFix,OrTok)
      case AndAndOp => f(AndAndFix,AndAndTok)
      case OrOrOp => f(OrOrFix,OrOrTok)
    }
  }
  def token(op: Option[AssignOp]): Token = op match {
    case None => EqTok
    case Some(op) => op match {
      case MulOp => MulEqTok
      case DivOp => DivEqTok
      case ModOp => ModEqTok
      case AddOp => PlusEqTok
      case SubOp => MinusEqTok
      case LShiftOp => LShiftEqTok
      case RShiftOp => RShiftEqTok
      case UnsignedRShiftOp => UnsignedRShiftEqTok
      case AndOp => AndEqTok
      case XorOp => XorEqTok
      case OrOp => OrEqTok
  }}

  // AST expressions
  implicit def prettyAExp(e: AExp): (Fixity,Tokens) = e match {
    case NameAExp(n) => pretty(n)
    case x: ALit => prettyALit(x)
    case ParenAExp(e,a) => around(e,a)
    case FieldAExp(e,t,f) => fix(FieldFix, left(_,e) ::: DotTok :: tokens(t) ::: tokens(f))
    case MethodRefAExp(e,t,f) => fix(FieldFix, left(_,e) ::: ColonColonTok :: tokens(t) ::: tokens(f))
    case NewRefAExp(e,t) => fix(FieldFix, left(_,e) ::: ColonColonTok :: tokens(t) ::: List(NewTok))
    case TypeApplyAExp(e,t) => fix(ApplyFix, left(_,e) ::: typeBracket(t))
    case NewAExp(t,e) => fix(NewFix, tokens(t) ::: List(NewTok) ::: right(_,e))
    case WildAExp(None) => (HighestFix, List(QuestionTok))
    case WildAExp(Some((b,t))) => fix(WildFix, QuestionTok :: token(b) :: right(_,t))
    case UnaryAExp(op,e) if isPrefix(op) => fix(PrefixFix, token(op) :: right(_,e))
    case UnaryAExp(op,e)               => fix(PostfixFix, left(_,e) ::: List(token(op)))
    case BinaryAExp(op,x,y) => { val (s,t) = pretty(op); (s, left(s,x) ::: t ::: right(s,y)) }
    case CastAExp(t,e) => (PrefixFix, parens(t) ::: right(PrefixFix,e))
    case CondAExp(c,t,f) => fix(CondFix, s => left(s,c) ::: QuestionTok :: tokens(t) ::: ColonTok :: right(s,f))
    case AssignAExp(op,x,y) => fix(AssignFix, s => left(s,x) ::: token(op) :: right(s,y))
    case ArrayAExp(xs,a) => around(xs,a)
    case ApplyAExp(e,xs,a) => {
      val s = a match { case NoAround => JuxtFix; case _ => ApplyFix }
      fix(s, left(_,e) ::: around(xs,a)._2)
    }
  }
  implicit def prettyATypeArgs(t: Option[KList[AExp]]): (Fixity,Tokens) =
    (HighestFix, t map (typeBracket(_)) getOrElse Nil)

  // AST statements
  implicit def prettyAStmt(s: AStmt): (Fixity,Tokens) = {
    def key[A](key: Token, x: Option[A])(implicit p: Pretty[A]): (Fixity,Tokens) =
      (SemiFix, key :: (x map (tokens(_)) getOrElse Nil) ::: List(SemiTok))
    s match {
      case EmptyAStmt => (SemiFix,List(SemiTok))
      case HoleAStmt => (HighestFix,List(HoleTok))
      case VarAStmt(m,t,v) => (SemiFix, m.map(tokens).flatten ::: tokens(t) ::: tokens(v))
      case BlockAStmt(b) => (HighestFix, LCurlyTok :: tokens(b) ::: List(RCurlyTok))
      case ExpAStmt(e) => (SemiFix, tokens(e) ::: List(SemiTok))
      case AssertAStmt(c,None) => (SemiFix, AssertTok :: tokens(c) ::: List(SemiTok))
      case AssertAStmt(c,Some(m)) => (SemiFix, AssertTok :: tokens(c) ::: ColonTok :: tokens(m) ::: List(SemiTok))
      case BreakAStmt(l) => key(BreakTok,l)
      case ContinueAStmt(l) => key(ContinueTok,l)
      case ReturnAStmt(e) => key(ReturnTok,e)
      case ThrowAStmt(e) => key(ThrowTok,Some(e))
      case SyncAStmt(e,b,a) => (SemiFix, SynchronizedTok :: around(e,a)._2 ::: tokens(b))
      case IfAStmt(c,x,a) => (SemiFix, IfTok :: around(c,a)._2 ::: tokens(x))
      case IfElseAStmt(c,x,y,a) => (SemiFix, IfTok :: around(c,a)._2 ::: tokens(x) ::: ElseTok :: tokens(y))
      case WhileAStmt(c,s,flip,a) => (SemiFix, whileUntil(flip) :: around(c,a)._2 ::: tokens(s))
      case DoAStmt(s,c,flip,a) => (SemiFix, DoTok :: tokens(s) ::: whileUntil(flip) :: around(c,a)._2 ::: List(SemiTok))
      case ForAStmt(i,s,a) => (SemiFix, ForTok :: around(i,a)._2 ::: tokens(s))
    }
  }
  def whileUntil(flip: Boolean): Token = if (flip) UntilTok else WhileTok
  implicit def prettyAStmts(ss: List[AStmt]): (Fixity,Tokens) = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyAVar(d: AVarDecl): (Fixity,Tokens) = d match {
    case (x,n,None) => prettyDims(x,n)
    case (x,n,Some(e)) => fix(AssignFix, prettyDims(x,n)._2 ::: EqTok :: right(_,e))
  }
  implicit def prettyFor(i: ForInfo): (Fixity,Tokens) = (LowestExpFix,i match {
    case For(i,c,u) => tokens(i) ::: tokens(c) ::: SemiTok :: tokens(CommaList(u))
    case Foreach(m,t,v,n,e) => m.map(tokens).flatten ::: tokens(t) ::: prettyDims(v,n)._2 ::: ColonTok :: tokens(e)
  })

  // Literals
  implicit def prettyALit(x: ALit) = (HighestFix, List(x match {
    case IntALit(v) => IntLitTok(v)
    case LongALit(v) => LongLitTok(v)
    case FloatALit(v) => FloatLitTok(v)
    case DoubleALit(v) => DoubleLitTok(v)
    case CharALit(v) => CharLitTok(v)
    case StringALit(v) => StringLitTok(v)
  }))

  // Keywords
  def token(b: Bound) = b match {
    case Extends => ExtendsTok
    case Super => SuperTok
  }
  def tokens(m: Mod): Tokens = m match {
    case Annotation(n) => AtTok :: tokens(n)
    case Abstract => List(AbstractTok)
    case Public => List(PublicTok)
    case Protected => List(ProtectedTok)
    case Private => List(PrivateTok)
    case Static => List(StaticTok)
    case Final => List(FinalTok)
    case Strictfp => List(StrictfpTok)
    case Transient => List(TransientTok)
    case Volatile => List(VolatileTok)
    case Synchronized => List(SynchronizedTok)
  }

  // Denotations
  implicit def prettyItem(i: Item)(implicit env: Env): (Fixity,Tokens) = {
    def relative(i: Item) = i match {
      case i:Member if i.parent != LocalPkg && i.parent != JavaLangPkg =>
        (FieldFix, tokens(i.parent) ::: DotTok :: tokens(i.name))
      case _ => pretty(i.name)
    }
    i match {
      // Types
      case i: RefTypeItem => relative(i)

      // Static members
      case i: EnumConstantItem => relative(i)
      case i: StaticFieldItem => relative(i)
      case i: MethodItem if i.isStatic => relative(i)

      // Non-static things ought to be fully resolved by the expression they're contained in (otherwise the denotation was wrong)
      case i: Item => pretty(i.name)
    }
  }

  implicit def prettyType(t: Type)(implicit env: Env): (Fixity,Tokens) = t match {
    case t:LangType => prettyLangType(t)
    case t:RefType => prettyRefType(t)
  }
  implicit def prettyTypeVar(t: TypeVar): (Fixity,Tokens) = pretty(t.name)
  implicit def prettyLangType(t: LangType): (Fixity,Tokens) = (HighestFix, List(t match {
    case VoidType    => VoidTok
    case BooleanType => BooleanTok
    case ByteType    => ByteTok
    case ShortType   => ShortTok
    case IntType     => IntTok
    case LongType    => LongTok
    case FloatType   => FloatTok
    case DoubleType  => DoubleTok
    case CharType    => CharTok
  }))
  implicit def prettyRefType(t: RefType)(implicit env: Env): (Fixity,Tokens) = {
    def cls(t: ClassType): (Fixity,Tokens) =
      if (t.args.isEmpty) pretty(t.item)
      else (ApplyFix, tokens(t.item) ::: LtTok :: tokens(CommaList(t.args)) ::: List(GtTok))

    t match {
      case NullType => pretty("nulltype")
      case t:ClassType => cls(t)
      case x:TypeVar => prettyTypeVar(x)
      case IntersectType(ts) => pretty(AndList(ts.toList))
      case ArrayType(t) => (ApplyFix, tokens(t) ::: List(LBrackTok,RBrackTok))
    }
  }
  implicit def prettyTypeArg(t: TypeArg)(implicit env: Env): (Fixity,Tokens) = {
    def wild(t: RefType, d: Token) = fix(WildFix, QuestionTok :: d :: right(_,t))
    t match {
      case t:RefType => pretty(t)
      case WildSub(t) => wild(t,ExtendsTok)
      case WildSuper(t) => wild(t,SuperTok)
    }
  }
  implicit def prettyLit(x: Lit) = (HighestFix, List(x match {
    case ByteLit(v,s) => IntLitTok(s)
    case ShortLit(v,s) => IntLitTok(s)
    case IntLit(v,s) => IntLitTok(s)
    case LongLit(v,s) => LongLitTok(s)
    case FloatLit(v,s) => FloatLitTok(s)
    case DoubleLit(v,s) => DoubleLitTok(s)
    case BooleanLit(b) => BoolLitTok(b)
    case CharLit(v,s) => CharLitTok(s)
    case StringLit(v,s) => StringLitTok(s)
    case NullLit => NullTok
  }))
  def prettyField(x: Exp, f: Item)(implicit env: Env): (Fixity,Tokens) =
    fix(FieldFix, left(_,x) ::: DotTok :: tokens(f.name))
  implicit def prettyExp(e: Exp)(implicit env: Env): (Fixity,Tokens) = e match {
    case l: Lit => pretty(l)
    case ParameterExp(x) => pretty(x)
    case LocalVariableExp(x) => pretty(x)
    case EnumConstantExp(None,f) => pretty(f)
    case EnumConstantExp(Some(x),f) => prettyField(x,f)
    case ThisExp(i) => if (env.inScope(i)) (HighestFix,List(ThisTok)) else (FieldFix, tokens(i.self) ::: DotTok :: List(ThisTok))
    case SuperExp(i) => if (env.inScope(i)) (HighestFix,List(SuperTok)) else (FieldFix, tokens(i.self) ::: DotTok :: List(SuperTok))
    case CastExp(t,x) => fix(PrefixFix, parens(t) ::: right(_,x))
    case ImpExp(op,x) if isPrefix(op) => fix(PrefixFix, token(op) :: right(_,x))
    case e:UnaryExp                   => fix(PostfixFix, left(_,e.e) ::: List(token(e.op)))
    case BinaryExp(op,x,y) => { val (s,t) = pretty(op); (s, left(s,x) ::: t ::: right(s,y)) }
    case AssignExp(op,x,y) => fix(AssignFix, s => left(s,x) ::: token(op) :: right(s,y))
    case ParenExp(x) => (HighestFix,parens(x))
    case ApplyExp(f,ts,a) => {
      val t = tokensTypeArgs(ts)
      def method(x: Exp, f: Item) = left(FieldFix,x) ::: DotTok :: t ::: tokens(f.name)
      (ApplyFix, (f match {
        case MethodDen(x,f) => method(x,f)
        case LocalMethodDen(f) => t ::: tokens(f) // TODO: ts goes in the middle of f?
        case StaticMethodDen(None,f) => t ::: tokens(f) // TODO: ts goes in the middle of f?
        case StaticMethodDen(Some(x),f) => method(x,f)
        case NewDen(c) => NewTok :: t ::: tokens(c.parent) // TODO: ts splits into two lists in different places
        case ForwardDen(c) => ThisTok :: t
        case DiscardCallableDen(_,_) => impossible
      }) ::: LParenTok :: separate(a.map(tokens(_)),List(CommaTok)) ::: List(RParenTok))
    }
    case FieldExp(x,f) => prettyField(x,f)
    case LocalFieldExp(f) => pretty(f)
    case StaticFieldExp(None,f) => pretty(f)
    case StaticFieldExp(Some(x),f) => prettyField(x,f)
    case IndexExp(e,i) => fix(ApplyFix, left(_,e) ::: LBrackTok :: tokens(i) ::: List(RBrackTok))
    case ArrayExp(t,xs) => (ApplyFix, NewTok :: tokens(ArrayType(t)) ::: prettyArrayExp(xs)._2)
    case EmptyArrayExp(t,is) => {
      def inner(t: Type, ts: Tokens): Tokens = t match {
        case ArrayType(t) => inner(t, ts ::: List(LBrackTok,RBrackTok))
        case t => tokens(t) ::: ts
      }
      def outer(t: Type, is: List[Exp], ts: Tokens): Tokens = (t,is) match {
        case (t,Nil) => inner(t,ts)
        case (ArrayType(t),i::is) => outer(t, is, ts ::: LBrackTok :: tokens(i) ::: List(RBrackTok))
        case _ => throw new RuntimeException("type mismatch (not enough array dimensions)")
      }
      (ApplyFix, NewTok :: outer(t,is,Nil))
    }
  }
  def tokensTypeArgs[A](ts: List[A])(implicit p: Pretty[A]): Tokens = ts match {
    case Nil => Nil
    case ts => LtTok :: separate(ts.map(tokens(_)),List(CommaTok)) ::: List(GtTok)
  }
  def tokensSig(f: Signature)(implicit env: Env): Tokens =
    tokensTypeArgs(f.tparams) ::: LParenTok :: tokens(CommaList(f.params)) ::: List(RParenTok)
  def prettyInit(e: Exp)(implicit env: Env): (Fixity,Tokens) = e match {
    case ArrayExp(_,xs) => prettyArrayExp(xs)
    case e => prettyExp(e)
  }
  def prettyArrayExp(xs: List[Exp])(implicit env: Env): (Fixity,Tokens) =
    (HighestFix, LCurlyTok :: tokens(CommaList(xs))(prettyList(_)(prettyInit)) ::: List(RCurlyTok))
  implicit def prettyStmt(s: Stmt)(implicit env: Env): (Fixity,Tokens) = s match {
    case EmptyStmt => (SemiFix, List(SemiTok))
    case HoleStmt => (HighestFix, List(HoleTok))
    case VarStmt(t,vs) => (SemiFix, tokens(t) ::: tokens(CommaList(vs)) ::: List(SemiTok))
    case ExpStmt(e) => (SemiFix, tokens(e) ::: List(SemiTok))
    case BlockStmt(b) => (HighestFix, LCurlyTok :: tokens(b) ::: List(RCurlyTok))
    case AssertStmt(c,None) => (SemiFix, AssertTok :: tokens(c) ::: List(SemiTok))
    case AssertStmt(c,Some(m)) => (SemiFix, AssertTok :: tokens(c) ::: ColonTok :: tokens(m) ::: List(SemiTok))
    case BreakStmt => (SemiFix, List(BreakTok,SemiTok))
    case ContinueStmt => (SemiFix, List(ContinueTok,SemiTok))
    case ReturnStmt(None) => (SemiFix, List(ReturnTok,SemiTok))
    case ReturnStmt(Some(e)) => (SemiFix, ReturnTok :: tokens(e) ::: List(SemiTok))
    case ThrowStmt(e) => (SemiFix, ThrowTok :: tokens(e) ::: List(SemiTok))
    case IfStmt(c,x) => (SemiFix, IfTok :: parens(c) ::: tokens(x))
    case IfElseStmt(c,x,y) => (SemiFix, IfTok :: parens(c) ::: tokens(x) ::: ElseTok :: tokens(y))
    case WhileStmt(c,x) => (SemiFix, WhileTok :: parens(c) ::: tokens(x))
    case DoStmt(x,c) => (SemiFix, DoTok :: tokens(x) ::: WhileTok :: parens(c) ::: List(SemiTok))
    case ForStmt(i,c,u,s) => (SemiFix, ForTok :: parens(
      tokens(i) ::: tokens(c) ::: SemiTok :: tokens(CommaList(u))) ::: tokens(s))
    case ForeachStmt(t,v,e,s) => (SemiFix, ForTok :: parens(
      tokens(t) ::: tokens(v) ::: ColonTok :: tokens(e)) ::: tokens(s))
    case SyncStmt(e,s) => (SemiFix, SynchronizedTok :: parens(e) ::: tokens(s))
    case _:DiscardStmt => impossible
  }
  implicit def prettyStmts(ss: List[Stmt])(implicit env: Env): (Fixity,Tokens) = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyVar(v: (LocalVariableItem,Dims,Option[Exp]))(implicit env: Env): (Fixity,Tokens) = v match {
    case (x,n,None) => prettyDims(x,n)
    case (x,n,Some(i)) => fix(AssignFix, prettyDims(x,n)._2 ::: EqTok :: right(_,i)(prettyInit))
  }
  implicit def prettyForInit(i: ForInit)(implicit env: Env): (Fixity,Tokens) = i match {
    case v: VarStmt => prettyStmt(v)
    case ForExps(es) => (SemiFix, tokens(CommaList(es)) ::: List(SemiTok))
  }
  implicit def prettyCallable(c: Callable)(implicit env: Env): (Fixity,Tokens) = {
    def method(x: Exp, f: Item) = fix(FieldFix,left(_,x) ::: DotTok :: tokens(f))
    c match {
      case MethodDen(x,f) => method(x,f)
      case LocalMethodDen(f) => pretty(f)
      case StaticMethodDen(None,f) => pretty(f)
      case StaticMethodDen(Some(x),f) => method(x,f)
      case NewDen(f) => (ApplyFix,NewTok :: tokens(f))
      case ForwardDen(f) => (HighestFix,List(ThisTok))
      case _:DiscardCallableDen => impossible
    }
  }
}