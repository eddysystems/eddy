package tarski

import tarski.AST._
import tarski.Arounds._
import tarski.Mods._
import tarski.Denotations._
import tarski.Items._
import tarski.Operators._
import tarski.Tokens._
import tarski.Types._
import utility.Locations._
import utility.Utility._

import scala.collection.mutable
import scala.language.implicitConversions

object Pretty {
  // Pretty only needs a small part of the environment
  abstract class Scope {
    def inScope(i: Item): Boolean
  }

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
  val FieldFix     = L("Field") // x.y
  val ApplyFix     = L("Apply") // x[y], x(y)
  val HighestFix   = N("Highest") // Parentheses, etc.

  // Top level pretty printing.  All of our tokens have attached location ranges.
  // The convention is that if a token came from the input stream, its range
  // should be the input range.  If a token was fabricated by us, its location
  // will be empty.
  type Tokens = List[Loc[Token]]
  type FixTokens = (Fixity,Tokens)
  type Pretty[-A] = A => FixTokens
  def pretty[A](x: A)(implicit p: Pretty[A]): FixTokens = p(x)
  def tokens[A](x: A)(implicit p: Pretty[A]): Tokens = p(x)._2

  // Pretty printing utilities
  val space = Loc(WhitespaceTok(" "),SRange.empty)
  implicit def prettyToken(x: Loc[Token]): FixTokens = (HighestFix,List(x))
  def around[A](x: A, a: Around)(implicit p: Pretty[A]): FixTokens = {
    val t = pretty(x)
    def left (g: Group) = g match { case Paren => LParenTok; case Brack => LBrackTok; case Curly => LCurlyTok }
    def right(g: Group) = g match { case Paren => RParenTok; case Brack => RBrackTok; case Curly => RCurlyTok }
    a match {
      case NoAround(_) => t
      case YesAround(l,r,a) => (HighestFix,Loc(left(l),a.l) :: t._2 ::: List(Loc(right(r),a.r)))
    }
  }
  def parens(x: Tokens, a: SGroup): Tokens =
    Loc(LParenTok,a.l) :: x ::: List(Loc(RParenTok,a.r))
  def parens[A](x: A, a: SGroup)(implicit p: Pretty[A]): Tokens =
    Loc(LParenTok,a.l) :: tokens(x) ::: List(Loc(RParenTok,a.r))
  def typeBracket[A](x: A, a: SGroup)(implicit p: Pretty[A]): Tokens =
    Loc(LtTok,a.l) :: tokens(x) ::: List(Loc(GtTok,a.r))
  private[this] val newLeft = Loc(LParenTok,SRange.empty)
  private[this] val newRight = List(Loc(RParenTok,SRange.empty))
  def parensIf[A](x: A, prec: Int)(implicit p: Pretty[A]): Tokens = {
    val (f,ts) = p(x)
    if (prec < f.prec) ts else newLeft :: ts ::: newRight
  }
  def toInt(b: Boolean): Int = if (b) 1 else 0
  def left [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec-toInt(slot.assoc==2))
  def right[A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec-toInt(slot.assoc==4))
  def non  [A](slot: Fixity, x: A)(implicit p: Pretty[A]): Tokens = parensIf(x,slot.prec)
  private def fix[A](s: Fixity, f: Fixity => A) = (s,f(s))

  // Names
  implicit def prettyName(x: Loc[Name]): FixTokens = (HighestFix,tokens(x.x,x.r))
  def prettyName(x: Name, r: SRange): FixTokens = (HighestFix,tokens(x,r))
  def prettyDims(x: Name, r: SRange, n: Dims): FixTokens = n match {
    case Nil => (HighestFix,tokens(x,r))
    case _ => (ApplyFix, tokens(x,r) ::: n.map(g => List(Loc(LBrackTok,g.l),Loc(RBrackTok,g.r))).flatten)
  }
  def tokens(x: Name, r: SRange): Tokens = List(Loc(IdentTok(x),r))

  // Options
  implicit def prettyOption[A](x: Option[A])(implicit p: Pretty[A]): FixTokens = x match {
    case None => Nil
    case Some(x) => p(x)
  }

  // Lists
  def separate[A](xs: List[A], s: Fixity, sep: Loc[Token])(implicit p: Pretty[A]): Tokens = xs match {
    case Nil => Nil
    case List(x) => pretty(x)._2
    case x::y => non(s,x) ::: sep :: separate(y,s,sep)
  }
  def separate[A <: HasRange](xs: List[A], s: Fixity, sep: Token)(implicit p: Pretty[A]): Tokens = xs match {
    case Nil => Nil
    case List(x) => pretty(x)._2
    case x::y => non(s,x) ::: Loc(sep,x.r.after) :: (separate[A](y,s,sep) : Tokens)
  }
  def separate[A](xs: List[A], s: Fixity, sep: Token, rs: List[SRange])(implicit p: Pretty[A]): Tokens = (xs,rs) match {
    case (Nil,Nil) => Nil
    case (List(x),Nil) => pretty(x)._2
    case (x::y,xr::yr) => non(s,x) ::: Loc(sep,xr) :: separate(y,s,sep,yr)
    case _ => impossible
  }
  def commas[A](xs: List[A], r: SRange)(implicit p: Pretty[A]): Tokens = separate(xs,CommaListFix,Loc(CommaTok,r))
  def commas[A <: HasRange](xs: List[A])(implicit p: Pretty[A]): Tokens = separate(xs,CommaListFix,CommaTok)
  def ands[A](xs: List[A], r: SRange)(implicit p: Pretty[A]): FixTokens = (AndListFix,separate(xs,AndListFix,Loc(AndTok,r)))
  def ands[A <: HasRange](xs: List[A])(implicit p: Pretty[A]): FixTokens = (AndListFix,separate(xs,AndListFix,AndTok))
  implicit def prettyList[A](k: KList[A])(implicit p: Pretty[A]): FixTokens = k match {
    case EmptyList => (LowestExpFix,Nil)
    case SingleList(x) => pretty(x)
    case CommaList2(xs,rs) => fix(CommaListFix,separate(xs,_,CommaTok,rs))
    case AndList2(xs,rs) => fix(AndListFix,separate(xs,_,AndTok,rs))
    case JuxtList(xs) => fix(JuxtFix,separate(xs,_,space))
  }

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
  def prettyBinary(op: BinaryOp, r: SRange): FixTokens = {
    def f(s: Fixity, t: Token) = (s,List(Loc(t,r)))
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
  implicit def prettyBinary(op: Loc[BinaryOp]): FixTokens = prettyBinary(op.x,op.r)
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
  implicit def prettyAExp(e: AExp): FixTokens = e match {
    case NameAExp(n,r) => pretty(Loc(n,r))
    case x:ALit => prettyALit(x)
    case ParenAExp(e,a) => around(e,a)
    case FieldAExp(e,dr,t,f,fr) => fix(FieldFix, left(_,e) ::: Loc(DotTok,dr) :: tokens(t) ::: tokens(f,fr))
    case MethodRefAExp(e,cc,t,f,fr) => fix(FieldFix, left(_,e) ::: Loc(ColonColonTok,cc) :: tokens(t) ::: tokens(f,fr))
    case NewRefAExp(e,cc,t,nr) => fix(FieldFix, left(_,e) ::: Loc(ColonColonTok,cc) :: tokens(t) ::: List(Loc(NewTok,nr)))
    case TypeApplyAExp(e,t,tr,true)  => fix(ApplyFix, left(_,e) ::: typeBracket(t,tr))
    case TypeApplyAExp(e,t,tr,false) => fix(ApplyFix, typeBracket(t,tr) ::: right(_,e))
    case NewAExp(nr,t,e) => fix(NewFix, Loc(NewTok,nr) :: tokens(t) ::: right(_,e))
    case WildAExp(qr,None) => (HighestFix, List(Loc(QuestionTok,qr)))
    case WildAExp(qr,Some(WildBound(b,br,t))) => fix(WildFix, Loc(QuestionTok,qr) :: Loc(token(b),br) :: right(_,t))
    case UnaryAExp(op,opr,e) if isPrefix(op) => fix(PrefixFix, Loc(token(op),opr) :: right(_,e))
    case UnaryAExp(op,opr,e)                 => fix(PostfixFix, left(_,e) ::: List(Loc(token(op),opr)))
    case BinaryAExp(op,opr,x,y) => { val (s,t) = prettyBinary(op,opr); (s, left(s,x) ::: t ::: right(s,y)) }
    case CastAExp(t,a,e) => (PrefixFix, around(t,a)._2 ::: right(PrefixFix,e))
    case CondAExp(c,qr,x,cr,y) => fix(CondFix, s => left(s,c) ::: Loc(QuestionTok,qr) :: tokens(x) ::: Loc(ColonTok,cr) :: right(s,y))
    case AssignAExp(op,opr,x,y) => fix(AssignFix, s => left(s,x) ::: space :: Loc(token(op),opr) :: space :: right(s,y))
    case ArrayAExp(xs,a) => around(xs,a)
    case ApplyAExp(e,xs,a) => {
      val s = a match { case NoAround(_) => JuxtFix; case _ => ApplyFix }
      fix(s, left(_,e) ::: around(xs,a)._2)
    }
    case InstanceofAExp(e,ir,t) => fix(RelFix,s => left(s,e) ::: Loc(InstanceofTok,ir) :: right(s,t))
  }
  implicit def prettyATypeArgs(t: Option[Grouped[KList[AExp]]]): FixTokens = (HighestFix, t match {
    case None => Nil
    case Some(Grouped(t,a)) => typeBracket(t,a)
  })

  // AST statements
  private[this] def hole(r: SRange) = List(Loc(HoleTok,r.after))
  implicit def prettyAStmt(s: AStmt): FixTokens = s match {
    case SemiAStmt(s,sr) => prettyAStmtHelper(s,sr)
    case _ => prettyAStmtHelper(s,s.r.after)
  }
  def prettyAStmtHelper(s: AStmt, sr: SRange): FixTokens = {
    val sem = List(Loc(SemiTok,sr))
    def key[A](key: Token, kr: SRange, x: Option[A])(implicit p: Pretty[A]): FixTokens =
      (SemiFix, Loc(key,kr) :: (x map (tokens(_)) getOrElse Nil) ::: sem)
    s match {
      case SemiAStmt(s,sr) => prettyAStmtHelper(s,sr)
      case EmptyAStmt(_) => (SemiFix,sem)
      case HoleAStmt(r) => (HighestFix,hole(r))
      case VarAStmt(m,t,v) => (SemiFix, m.map(tokens).flatten ::: tokens(t) ::: space :: tokens(v) ::: sem)
      case BlockAStmt(b,a) => (HighestFix, Loc(LCurlyTok,a.l) :: tokens(b) ::: List(Loc(RCurlyTok,a.r)))
      case TokAStmt(b,r) => (HighestFix, List(Loc(b,r)))
      case ExpAStmt(e) => (SemiFix, tokens(e) ::: sem)
      case AssertAStmt(ar,c,None) => (SemiFix, Loc(AssertTok,ar) :: tokens(c) ::: sem)
      case AssertAStmt(ar,c,Some((cr,m))) => (SemiFix, Loc(AssertTok,ar) :: tokens(c) ::: Loc(ColonTok,cr) :: tokens(m) ::: sem)
      case BreakAStmt(br,l) => key(BreakTok,br,l)
      case ContinueAStmt(cr,l) => key(ContinueTok,cr,l)
      case ReturnAStmt(rr,e) => key(ReturnTok,rr,e)
      case ThrowAStmt(tr,e) => key(ThrowTok,tr,Some(e))
      case SyncAStmt(sr,e,a,b) => (SemiFix, Loc(SynchronizedTok,sr) :: around(e,a)._2 ::: tokens(b))
      case IfAStmt(ir,c,a,x) => (SemiFix, Loc(IfTok,ir) :: around(c,a)._2 ::: tokens(x))
      case IfElseAStmt(ir,c,a,x,er,y) => (SemiFix, Loc(IfTok,ir) :: around(c,a)._2 ::: tokens(x) ::: Loc(ElseTok,er) :: tokens(y))
      case WhileAStmt(wr,flip,c,a,s) => (SemiFix, whileUntil(wr,flip) :: around(c,a)._2 ::: tokens(s))
      case DoAStmt(dr,s,wr,flip,c,a) => (SemiFix, Loc(DoTok,dr) :: tokens(s) ::: whileUntil(wr,flip) :: around(c,a)._2 ::: sem)
      case ForAStmt(fr,i,a,s) => (SemiFix, Loc(ForTok,fr) :: around(i,a)._2 ::: tokens(s))
    }
  }
  def whileUntil(r: SRange, flip: Boolean): Loc[Token]= Loc(if (flip) UntilTok else WhileTok,r)
  implicit def prettyAStmts(ss: List[AStmt]): FixTokens = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyAVar(d: AVarDecl): FixTokens = d match {
    case AVarDecl(x,xr,n,None) => prettyDims(x,xr,n)
    case AVarDecl(x,xr,n,Some((eqr,e))) => fix(AssignFix, prettyDims(x,xr,n)._2 ::: space :: Loc(EqTok,eqr) :: space :: right(_,e))
  }
  implicit def prettyFor(i: ForInfo): FixTokens = (LowestExpFix,i match {
    case For(i,_,c,r1,u) => tokens(i) ::: tokens(c) ::: Loc(SemiTok,r1) :: tokens(u)
    case Foreach(m,t,v,vr,n,cr,e) => m.map(tokens).flatten ::: tokens(t) ::: prettyDims(v,vr,n)._2 ::: Loc(ColonTok,cr) :: tokens(e)
  })

  // Literals
  implicit def prettyALit(x: ALit): FixTokens = (HighestFix, List(Loc(x match {
    case IntALit(v,_) => IntLitTok(v)
    case LongALit(v,_) => LongLitTok(v)
    case FloatALit(v,_) => FloatLitTok(v)
    case DoubleALit(v,_) => DoubleLitTok(v)
    case CharALit(v,_) => CharLitTok(v)
    case StringALit(v,_) => StringLitTok(v)
  },x.r)))

  // Keywords
  def token(b: Bound) = b match {
    case Extends => ExtendsTok
    case Super => SuperTok
  }
  def tokens(m: Loc[Mod]): Tokens = m.x match {
    case Annotation(ar,n,nr) => Loc(AtTok,ar) :: tokens(n,nr)
    case s:SimpleMod => List(Loc(s match {
      case Abstract => AbstractTok
      case Public => PublicTok
      case Protected => ProtectedTok
      case Private => PrivateTok
      case Static => StaticTok
      case Final => FinalTok
      case Strictfp => StrictfpTok
      case Transient => TransientTok
      case Volatile => VolatileTok
      case Synchronized => SynchronizedTok
    },m.r))
  }

  // Items
  implicit def prettyItem(i: Loc[Item])(implicit env: Scope): FixTokens = prettyItem(i.x)(env,i.r)
  implicit def prettyItem(i: Item)(implicit env: Scope, r: SRange): FixTokens =
    if (env.inScope(i)) prettyName(i.name,r)
    else i match {
      case i:Member if i.parent != LocalPkg && i.parent != Base.JavaLangPkg =>
        (FieldFix, tokens(i.parent) ::: Loc(DotTok,r) :: tokens(i.name,r))
      case _ => prettyName(i.name,r) // We can't see this item, show it anyway
    }
  implicit def prettyParentItem(i: Item with Parent)(implicit env: Scope, r: SRange): FixTokens = prettyItem(i)

  // Types
  implicit def prettyType(t: Loc[Type])(implicit env: Scope): FixTokens = prettyType(t.x)(env,t.r)
  implicit def prettyType(t: Type)(implicit env: Scope, r: SRange): FixTokens = t match {
    case t:LangType => prettyLangType(t)
    case t:RefType => prettyRefType(t)
  }
  implicit def prettyTypeVar(t: TypeVar)(implicit r: SRange): FixTokens = prettyName(t.name,r)
  implicit def prettyLangType(t: LangType)(implicit r: SRange): FixTokens = (HighestFix, List(Loc(t match {
    case VoidType    => VoidTok
    case BooleanType => BooleanTok
    case ByteType    => ByteTok
    case ShortType   => ShortTok
    case IntType     => IntTok
    case LongType    => LongTok
    case FloatType   => FloatTok
    case DoubleType  => DoubleTok
    case CharType    => CharTok
  },r)))
  implicit def prettyRefType(t: RefType)(implicit env: Scope, r: SRange): FixTokens = t match {
    case NullType => prettyName("nulltype",r)
    case t:ClassType => prettyClassType(t)
    case x:TypeVar => prettyTypeVar(x)
    case IntersectType(ts) => ands(ts.toList,r)
    case ArrayType(t) => (ApplyFix, tokens(t) ::: List(Loc(LBrackTok,r),Loc(RBrackTok,r)))
    case _ => impossible // Otherwise, Scala warns about nonexhaustive match for _: this.<local child>
  }
  implicit def prettyClassType(t: ClassType)(implicit env: Scope, r: SRange): FixTokens =
    if (t.args.isEmpty) pretty(t.item)
    else (ApplyFix, tokens(t.item) ::: Loc(LtTok,r) :: commas(t.args,r) ::: List(Loc(GtTok,r)))
  implicit def prettyTypeArg(t: TypeArg)(implicit env: Scope, r: SRange): FixTokens = {
    val question = Loc(QuestionTok,r)
    def wild(t: RefType, d: Token): FixTokens = fix(WildFix, question :: Loc(d,r) :: right(_,t))
    t match {
      case t:RefType => pretty(t)
      case WildSub(t) if t == ObjectType => (HighestFix,List(question))
      case WildSub(t) => wild(t,ExtendsTok)
      case WildSuper(t) => wild(t,SuperTok)
    }
  }
  implicit def prettyParent(p: Parent)(implicit env: Scope, r: SRange): FixTokens = p match {
    case t:ClassType => prettyClassType(t)
    case t:SimpleParent => pretty(t.item)
  }

  // Denotations
  implicit def prettyLit(x: Lit) = (HighestFix, List(Loc(x match {
    case ByteLit(v,s,_) => IntLitTok(s)
    case ShortLit(v,s,_) => IntLitTok(s)
    case IntLit(v,s,_) => IntLitTok(s)
    case LongLit(v,s,_) => LongLitTok(s)
    case FloatLit(v,s,_) => FloatLitTok(s)
    case DoubleLit(v,s,_) => DoubleLitTok(s)
    case BooleanLit(b,_) => BoolLitTok(b)
    case CharLit(v,s,_) => CharLitTok(s)
    case StringLit(v,s,_) => StringLitTok(s)
    case NullLit(_) => NullTok
  },x.r)))
  def prettyField(x: Exp, dot: SRange, f: Item, fr: SRange)(implicit env: Scope): FixTokens =
    fix(FieldFix, left(_,x) ::: Loc(DotTok,dot) :: tokens(f.name,fr))
  implicit def prettyExp(e: Exp)(implicit env: Scope): FixTokens = e match {
    case l:Lit => pretty(l)
    case LocalExp(x,r) => prettyItem(x)(env,r)
    case ThisExp(i,r) => implicit val r_ = r
                         if (env.inScope(i)) (HighestFix,List(Loc(ThisTok,r)))
                         else (FieldFix, tokens(i.item) ::: Loc(DotTok,r) :: List(Loc(ThisTok,r)))
    case SuperExp(i,r) => implicit val r_ = r
                          if (env.inScope(i)) (HighestFix,List(Loc(SuperTok,r)))
                          else (FieldFix, tokens(i.ty) ::: List(Loc(DotTok,r),Loc(SuperTok,r)))
    case CastExp(t,a,x) => implicit val tr = a.r
                           fix(PrefixFix, parens(t,a) ::: right(_,x))
    case e:UnaryExp if isPrefix(e.op) => fix(PrefixFix, Loc(token(e.op),e.opr) :: right(_,e.e))
    case e:UnaryExp                   => fix(PostfixFix, left(_,e.e) ::: List(Loc(token(e.op),e.opr)))
    case BinaryExp(op,opr,x,y) => { val (s,t) = prettyBinary(op,opr); (s, left(s,x) ::: t ::: right(s,y)) }
    case AssignExp(op,opr,x,y) => fix(AssignFix, s => left(s,x) ::: space :: Loc(token(op),opr) :: space :: right(s,y))
    case ParenExp(x,a) => (HighestFix,parens(x,a))
    case ApplyExp(f,as,a,_) => (ApplyFix, tokens(f) ::: parens(commas(as),a))
    case FieldExp(None,f,fr) => prettyItem(f)(env,fr)
    case e@FieldExp(Some(x),f,fr) => prettyField(x,e.dot,f,fr)
    case IndexExp(e,i,a) => fix(ApplyFix, left(_,e) ::: Loc(LBrackTok,a.l) :: tokens(i) ::: List(Loc(RBrackTok,a.r)))
    case ArrayExp(t,xs,a) => implicit val tr = a.r
                             (ApplyFix, Loc(NewTok,a.l) :: tokens(ArrayType(t)) ::: prettyArrayExp(xs,a)._2)
    case EmptyArrayExp(t,is) => {
      val lr = is.head.a.l
      val rr = is.last.a.r
      def inner(t: Type, ts: Tokens): Tokens = t match {
        case ArrayType(t) => inner(t, ts ::: List(Loc(LBrackTok,rr),Loc(RBrackTok,rr)))
        case t => implicit val tr = lr
                  tokens(t) ::: ts
      }
      def outer(t: Type, is: List[Grouped[Exp]], ts: Tokens): Tokens = (t,is) match {
        case (t,Nil) => inner(t,ts)
        case (ArrayType(t),Grouped(i,a)::is) => outer(t, is, ts ::: Loc(LBrackTok,a.l) :: tokens(i) ::: List(Loc(RBrackTok,a.r)))
        case _ => throw new RuntimeException("Type mismatch (not enough array dimensions)")
      }
      (ApplyFix, Loc(NewTok,lr) :: outer(t,is,Nil))
    }
    case CondExp(c,qr,x,cr,y,_) => fix(CondFix, s => left(s,c) ::: Loc(QuestionTok,qr) :: tokens(x) ::: Loc(ColonTok,cr) :: right(s,y))
  }
  implicit def prettyCallable(call: Callable)(implicit env: Scope): FixTokens = {
    def method[A](x: A, dot: SRange, f: Item, fr: SRange)(implicit p: Pretty[A]): FixTokens =
      fix(FieldFix,left(_,x) ::: Loc(DotTok,dot) :: tokens(f.name,fr))
    def methodTs[A](x: A, dot: SRange, ts: List[TypeArg], a: SGroup, f: Item, fr: SRange)(implicit p: Pretty[A]): FixTokens =
      fix(FieldFix,left(_,x) ::: Loc(DotTok,dot) :: tokensTypeArgs(ts,a) ::: tokens(f.name,fr))
    def gnu(nr: SRange, p: Option[ClassType], c: ConstructorItem, cr: SRange,
            ts0: Option[Grouped[List[TypeArg]]], ts1: List[TypeArg], a1: => SGroup): FixTokens = {
      (NewFix,Loc(NewTok,nr) :: tokensTypeArgs(ts1,a1) ::: (p match {
        case None => Nil
        case Some(p) => tokens(p)(prettyType(_)(env,cr)) ::: List(Loc(DotTok,cr))
      }) ::: prettyItem(c.parent)(env,cr)._2 ::: tokensTypeArgs(ts0))
    }
    def forward(x: ThisOrSuper, xr: SRange, c: ConstructorItem, ts: Option[Grouped[List[TypeArg]]]) = {
      val forward = Loc(x match {
        case _:ThisItem => ThisTok
        case _:SuperItem => SuperTok
      },xr)
      (ApplyFix,tokensTypeArgs(ts) ::: List(forward))
    }
    (call match {
      case TypeApply(f,_,_,true) => f // Don't print hidden type args
      case f => f
    }) match {
      case           LocalMethodDen(f,fr) => prettyItem(f)(env,fr)
      case TypeApply(LocalMethodDen(f,fr),ts,a,_) => methodTs(Loc(ThisTok,a.l),a.l,ts,a,f,fr)
      case           m@MethodDen(Some(x),f,fr) => method(x,m.dot,f,fr)
      case TypeApply(m@MethodDen(Some(x),f,fr),ts,a,_) => methodTs(x,m.dot,ts,a,f,fr)
      case           MethodDen(None,f,fr) => prettyItem(f)(env,fr)
      case TypeApply(MethodDen(None,f,fr),ts,a,_) => (FieldFix,prettyItem(f.parent)(env,fr)._2 ::: Loc(DotTok,fr)
                                                            :: tokensTypeArgs(ts,a) ::: tokens(f.name,fr))
      case           NewDen(nr,p,c,cr,ts0) => gnu(nr,p,c,cr,ts0,Nil,impossible)
      case TypeApply(NewDen(nr,p,c,cr,ts0),ts1,a1,_) => gnu(nr,p,c,cr,ts0,ts1,a1)
      case           ForwardDen(x,xr,c) => forward(x,xr,c,None)
      case TypeApply(ForwardDen(x,xr,c),ts,a,_) => forward(x,xr,c,Some(Grouped(ts,a)))
      case           DiscardCallableDen(ds,f) => above(ds,f)
      case TypeApply(DiscardCallableDen(ds,f),ts,a,h) => above(ds,TypeApply(f,ts,a,h))
    }
  }
  def tokensTypeArgs(ts: List[TypeArg], a: => SGroup)(implicit env: Scope): Tokens = ts match {
    case Nil => Nil
    case ts => implicit val tr = a.r
               Loc(LtTok,a.l) :: commas(ts,tr) ::: List(Loc(GtTok,a.r))
  }
  def tokensTypeArgs(ts: Option[Grouped[List[TypeArg]]])(implicit env: Scope): Tokens = ts match {
    case None => Nil
    case Some(Grouped(ts,a)) => tokensTypeArgs(ts,a)
  }
  def prettyInit(e: Exp)(implicit env: Scope): FixTokens = e match {
    case ArrayExp(_,xs,a) => prettyArrayExp(xs,a)
    case e => prettyExp(e)
  }
  def prettyArrayExp(xs: List[Exp], a: SGroup)(implicit env: Scope): FixTokens =
    (HighestFix, Loc(LCurlyTok,a.l) :: commas(xs)(prettyInit) ::: List(Loc(RCurlyTok,a.r)))
  implicit def prettyStmt(s: Stmt)(implicit env: Scope): FixTokens = prettyStmtHelper(s,s.r.after)
  def prettyStmtHelper(s: Stmt, sr: SRange)(implicit env: Scope): FixTokens = {
    val sem = List(Loc(SemiTok,sr))
    s match {
      case SemiStmt(x,sr) => prettyStmtHelper(x,sr)
      case EmptyStmt(r) => (SemiFix, sem)
      case HoleStmt(r) => (HighestFix, hole(r))
      case VarStmt(t,tr,vs,m) => implicit val tr_ = tr
                                 (SemiFix, m.map(tokens).flatten ::: tokens(t) ::: space :: commas(vs) ::: sem)
      case ExpStmt(e) => (SemiFix, tokens(e) ::: sem)
      case BlockStmt(b,a) => (HighestFix, Loc(LCurlyTok,a.l) :: tokens(b) ::: List(Loc(RCurlyTok,a.r)))
      case TokStmt(t,r) => (HighestFix, List(Loc(t,r)))
      case AssertStmt(ar,c,None) => (SemiFix, Loc(AssertTok,ar) :: tokens(c) ::: sem)
      case AssertStmt(ar,c,Some((cr,m))) => (SemiFix, Loc(AssertTok,ar) :: tokens(c) ::: Loc(ColonTok,cr) :: tokens(m) ::: sem)
      case BreakStmt(br,lab)    => (SemiFix, Loc(BreakTok,br)    :: tokens(lab) ::: sem)
      case ContinueStmt(cr,lab) => (SemiFix, Loc(ContinueTok,cr) :: tokens(lab) ::: sem)
      case ReturnStmt(rr,None) => (SemiFix, Loc(ReturnTok,rr) :: sem)
      case ReturnStmt(rr,Some(e)) => (SemiFix, Loc(ReturnTok,rr) :: tokens(e) ::: sem)
      case ThrowStmt(tr,e) => (SemiFix, Loc(ThrowTok,tr) :: tokens(e) ::: sem)
      case IfStmt(ir,c,a,x) => (SemiFix, Loc(IfTok,ir) :: parens(c,a) ::: tokens(x))
      case IfElseStmt(ir,c,a,x,er,y) => (SemiFix, Loc(IfTok,ir) :: parens(c,a) ::: tokens(x) ::: Loc(ElseTok,er) :: tokens(y))
      case WhileStmt(wr,c,a,x) => (SemiFix, Loc(WhileTok,wr) :: parens(c,a) ::: tokens(x))
      case DoStmt(dr,x,wr,c,a) => (SemiFix, Loc(DoTok,dr) :: tokens(x) ::: Loc(WhileTok,wr) :: parens(c,a) ::: sem)
      case ForStmt(fr,i,c,sr,u,a,s) => (SemiFix, Loc(ForTok,fr) :: parens(
        tokens(i) ::: tokens(c) ::: Loc(SemiTok,sr) :: commas(u),a) ::: tokens(s))
      case ForeachStmt(fr,m,t,tr,v,vr,e,a,s) => (SemiFix, Loc(ForTok,fr) :: parens(
        m.map(tokens).flatten ::: prettyType(t)(env,tr)._2 ::: tokens(v.name,vr) ::: Loc(ColonTok,vr) :: tokens(e),a) ::: tokens(s))
      case SyncStmt(sr,e,a,s) => (SemiFix, Loc(SynchronizedTok,sr) :: parens(e,a) ::: tokens(needBlock(s)))
      case TryStmt(tr,s,cs,f) => notImplemented // Make sure to call needBlock
      case CommentStmt(t,r) => (HighestFix,List(Loc(t,r)))
      case _:DiscardStmt => impossible
    }
  }
  implicit def prettyStmts(ss: List[Stmt])(implicit env: Scope): FixTokens = (SemiFix, ss.map(tokens(_)).flatten)
  implicit def prettyVar(v: VarDecl)(implicit env: Scope): FixTokens = v match {
    case VarDecl(x,xr,n,None) => prettyDims(x.name,xr,n)
    case VarDecl(x,xr,n,Some((eq,i))) => fix(AssignFix, prettyDims(x.name,xr,n)._2
      ::: space :: Loc(EqTok,eq) :: space :: right(_,i)(prettyInit))
  }
  implicit def prettyForInit(i: ForInit)(implicit env: Scope): FixTokens = i match {
    case v: VarStmt => prettyStmt(v)
    case ForExps(es,sr) => (SemiFix, commas(es) ::: List(Loc(SemiTok,sr)))
  }
  implicit def prettyVarStmt(v: VarStmt)(implicit env: Scope): FixTokens = prettyStmt(v)

  // Print a type variable with bound details
  def details(v: TypeVar)(implicit env: Scope, f: ShowFlags): String = {
    implicit val tr = SRange.unknown
    val mid = v.name
    val pre = if (v.lo == NullType) mid else s"${show(v.lo)} extends $mid"
    if (v.hi == ObjectType) pre else s"$pre extends ${show(v.hi)}"
  }

  // For debugging use only.  The user should never see.
  def above[A](ds: List[Stmt], x: A)(implicit p: Pretty[A], env: Scope): FixTokens = ds match {
    case Nil => pretty(x)
    case ds =>
      val r = SRange.unknown
      val semi = Loc(SemiTok,r)
      (HighestFix,Loc(IdentTok("Above"),r) :: Loc(LParenTok,r) ::
        separate(ds,SemiFix,SemiTok) ::: Loc(SemiTok,r) :: tokens(x) ::: List(Loc(RParenTok,r)))
  }
  implicit def prettyDen(x: Den)(implicit env: Scope): FixTokens = x match {
    case x:Exp => prettyExp(x)
    case p:PackageDen => prettyItem(p.p)(env,SRange.unknown)
    case x:Callable => prettyCallable(x)
    case TypeDen(ds,t) => implicit val tr = SRange.unknown; above(ds,t)
  }
}
