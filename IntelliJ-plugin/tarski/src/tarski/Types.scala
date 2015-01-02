package tarski

import ambiguity.Utility._
import tarski.AST._
import tarski.Base._
import tarski.Constants._
import tarski.Denotations.Exp
import tarski.Items._
import tarski.Operators._

import scala.annotation.tailrec

// Properties of types according to the Java spec, without extra intelligence
object Types {

  // Types
  sealed abstract class Type {
    def item: TypeItem
    def supers: List[RefType] // Immediate super classes
    def isSimple: Boolean // Do we depend on any type parameters?
    def isFinal: Boolean

    // Can we unbox to a primitive type?
    def unbox: Option[PrimType] = None
    def unboxNumeric: Option[NumType] = None
    def unboxIntegral: Option[IntegralType] = None
    def unboxesToNumeric: Boolean = false
    def unboxesToBoolean: Boolean = false

    // Does a type contain no raw variable?  I.e., is every type variable known?
    def known(implicit env: Tenv): Boolean

    // Substitute type parameters in a type.
    // Substitution is intentionally *not* recursive.  Each type parameter is substituted once and only once.
    // I believe this will make recursive generic function callers easier to handle.
    def substitute(implicit env: Tenv): Type

    // Make sure a type can be written safely in Java (converts nulltype to Object)
    def safe: Option[Type]

    // If we're generic, become raw
    def raw: Type
  }
  sealed abstract class LangType extends Type { // Primitive or void
    def supers = Nil
    def isSimple = true
    def isFinal = true
    def known(implicit env: Tenv) = true
    def substitute(implicit env: Tenv): this.type = this
    def safe = Some(this)
    def raw = this
  }
  case object VoidType extends LangType {
    def item = ubVoidItem
  }

  // Primitive types
  sealed abstract class PrimType extends LangType {
    override def unbox = Some(this)
    def box: SimpleType
    def isIntegral: Boolean
    def isNumeric: Boolean
    def isBoolean: Boolean
  }
  case object BooleanType extends PrimType {
    def item = ubBooleanItem
    def box = BooleanItem.simple
    override def unboxesToBoolean = true
    def isNumeric = false
    def isIntegral = false
    def isBoolean = true
  }
  sealed abstract class NumType extends PrimType {
    override def unboxNumeric = Some(this)
    override def unboxesToNumeric = true
    def isNumeric = true
    def isBoolean = false
  }
  sealed abstract class IntegralType extends NumType {
    override def unboxIntegral = Some(this)
    def isIntegral = true
  }
  sealed abstract class FloatingType extends NumType { def isIntegral = false }
  case object ByteType    extends IntegralType { def box = ByteItem.simple;      def item = ubByteItem }
  case object ShortType   extends IntegralType { def box = ShortItem.simple;     def item = ubShortItem }
  case object IntType     extends IntegralType { def box = IntegerItem.simple;   def item = ubIntItem }
  case object LongType    extends IntegralType { def box = LongItem.simple;      def item = ubLongItem }
  case object FloatType   extends FloatingType { def box = FloatItem.simple;     def item = ubFloatItem }
  case object DoubleType  extends FloatingType { def box = DoubleItem.simple;    def item = ubDoubleItem }
  case object CharType    extends IntegralType { def box = CharacterItem.simple; def item = ubCharItem }

  // Parents of classes (either classes or packages, or callables for local classes)
  // Inherited by SimpleItem and ClassType
  sealed trait Parent {
    def item: ParentItem
    def env: Tenv
    def isRaw: Boolean
    def isSimple: Boolean
    def known(implicit env: Tenv): Boolean
    def substitute(implicit env: Tenv): Parent
    def safe: Option[Parent]
    def raw: Parent // If we're generic, become raw
  }
  sealed trait GenericParent extends Parent {
    def item: ParentItem with GenericItem
    def args: List[TypeArg]
  }
  trait SimpleParent extends Parent { // Exists so that we can seal Parent
    def item: SimpleParentItem
    def env: Tenv = Map.empty
    def isRaw = false
    def isSimple = true
    def known(implicit env: Tenv) = true
    def substitute(implicit env: Tenv): this.type = this
    def safe: Option[Parent] = Some(this)
    def raw = this
  }

  // Reference types
  sealed abstract class RefType extends Type with TypeArg {
    def item: RefTypeItem
    def substitute(implicit env: Tenv): RefType
    def safe: Option[RefType]
    def raw: RefType
  }

  // Class types are either Object, simple, raw, or generic
  sealed abstract class ClassType extends RefType with GenericParent {
    def item: ClassItem
    def parent: Parent
    def base: ClassType = item.base.substitute(env)
    def supers = item.supers map (_.substitute(env))
    def isFinal = item.isFinal
    def substitute(implicit env: Tenv): ClassType
    def safe: Option[ClassType]
    def raw: ClassType
  }
  case object ObjectType extends ClassType {
    def item = ObjectItem
    def args = Nil
    def parent = JavaLangPkg
    def env = Map.empty
    override def supers = Nil
    def isRaw = false
    def isSimple = true
    def known(implicit env: Tenv) = true
    def substitute(implicit env: Tenv): this.type = this
    def safe = Some(this)
    def raw = this
  }
  case class SimpleType(item: ClassItem, parent: Parent) extends ClassType {
    def args = Nil
    def env = parent.env
    def isRaw = parent.isRaw
    def isSimple = parent.isSimple
    override def unbox = item.unbox
    def known(implicit env: Tenv) = parent.known
    def substitute(implicit env: Tenv) = SimpleType(item,parent.substitute)
    def safe = parent.safe map (SimpleType(item,_))
    def raw = SimpleType(item,parent.raw)
  }
  case class RawType(item: ClassItem, parent: Parent) extends ClassType {
    def args = Nil
    def env = item.tparams.foldLeft(parent.env)((env,p) => env+((p,None)))
    def isRaw = true
    def isSimple = parent.isSimple
    def known(implicit env: Tenv) = parent.known
    def substitute(implicit env: Tenv) = RawType(item,parent.substitute)
    def safe = parent.safe map (RawType(item,_))
    def raw = this
  }
  case class GenericType(item: ClassItem, args: List[TypeArg], parent: Parent) extends ClassType {
    def env() = capture(item.tparams,args,parent.env)._1
    def isRaw = parent.isRaw
    def isSimple = false
    def known(implicit env: Tenv) = args.forall(_.known) && parent.known
    def substitute(implicit env: Tenv) = {
      val p = parent.substitute
      if (!p.isRaw && (args forall (_.known))) GenericType(item,args map (_.substitute),p)
      else RawType(item,p)
    }
    def safe = for (p <- parent.safe; a <- allSome(args map (_.safe))) yield GenericType(item,a,p)
    def raw = RawType(item,parent.raw)
  }

  // Type arguments are either reference types or wildcards.  4.5.1
  sealed trait TypeArg { // Inherited by RefType and Wildcard
    def known(implicit env: Tenv): Boolean
    def substitute(implicit env: Tenv): TypeArg
    def safe: Option[TypeArg]
  }
  sealed trait Wildcard extends TypeArg {
    val t: RefType
    def known(implicit env: Tenv) = t.known
  }
  case class WildSub(t: RefType = ObjectType) extends Wildcard {
    def substitute(implicit env: Tenv) = WildSub(t.substitute)
    def safe = t.safe map WildSub
  }
  case class WildSuper(t: RefType) extends Wildcard {
    def substitute(implicit env: Tenv) = WildSuper(t.substitute)
    def safe = t.safe map WildSuper
  }

  // Nonclass reference types: null, type variables, intersection types, and arrays
  case object NullType extends RefType {
    def item = NoTypeItem
    def supers = Nil
    def isFinal = true
    def isSimple = true
    def known(implicit env: Tenv) = true
    def substitute(implicit env: Tenv): this.type = this
    def safe = Some(ObjectType) // nulltype becomes Object
    def raw = this
  }
  abstract class TypeVar extends RefType with RefTypeItem with RefEq {
    def name: Name
    def lo: RefType // Lower bound (e.g., nulltype)
    def hi: RefType // Upper bound (e.g., Object)

    def supers = List(hi)
    def item = this
    def isFinal = false
    def isSimple = false
    def known(implicit env: Tenv): Boolean = env.get(this) match {
      case Some(None) => false // We're raw, and therefore not known
      case _ => true
    }
    def substitute(implicit env: Tenv): RefType = env.get(this) match {
      case None => this
      case Some(Some(t)) => t
      case Some(None) => hi.substitute // if we encounter a type variable in a raw type, substitute its upper bound
    }
    def safe = Some(this)
    def simple = this
    def inside = this
    def raw: TypeVar = this
    def qualifiedName: Option[String] = None
  }
  case class IntersectType(ts: Set[RefType]) extends RefType {
    def item = NoTypeItem
    def supers = ts.toList flatMap (_.supers)
    def isFinal = false
    def isSimple = ts forall (_.isSimple)
    def known(implicit env: Tenv) = ts forall (_.known)
    def substitute(implicit env: Tenv) = IntersectType(ts map (_.substitute))
    def safe = allSome(ts map (_.safe)) map IntersectType
    def raw = IntersectType(ts map (_.raw))
  }
  case class ArrayType(t: Type) extends RefType {
    def item = ArrayItem
    def supers = CloneableItem.simple :: SerializableItem.simple :: (t match {
      case t: RefType => t.supers map ArrayType
      case _ => Nil
    })
    def isFinal = t.isFinal
    def isSimple = t.isSimple
    def known(implicit env: Tenv) = t.known
    def substitute(implicit env: Tenv) = ArrayType(t.substitute)
    def safe = t.safe map ArrayType
    def raw = ArrayType(t.raw)
  }

  // Type environments
  // None means the type variable is "raw" and therefore unknown.
  type Tenv = Map[TypeVar,Option[RefType]]

  // Basic reference types
  val StringType       = StringItem.simple
  val CloneableType    = CloneableItem.simple
  val SerializableType = SerializableItem.simple

  // Unary and binary numeric promotion (without unboxing logic)
  def promote(t: NumType): NumType = t match {
    case ByteType|ShortType|CharType => IntType
    case _ => t
  }
  def promote(t0: NumType, t1: NumType) = (t0,t1) match {
    case (_,DoubleType) |(DoubleType,_)  => DoubleType
    case (_,FloatType)  |(FloatType,_)   => FloatType
    case (_,LongType)   |(LongType,_)    => LongType
    case _                               => IntType
  }

  // Types of unary and binary expressions
  def unaryType(op: UnaryOp, t: Type): Option[Type] = op match {
    case PosOp|NegOp|PreIncOp|PreDecOp|PostIncOp|PostDecOp => t.unboxNumeric map promote
    case CompOp => t.unboxIntegral map promote
    case NotOp => if (t.unboxesToBoolean) Some(BooleanType) else None
  }
  def binaryType(op: BinaryOp, t0: Type, t1: Type): Option[Type] = op match {
    case AddOp if t0==StringType || t1==StringType => Some(StringType)
    case MulOp|DivOp|ModOp|AddOp|SubOp => for (n0 <- t0.unboxNumeric; n1 <- t1.unboxNumeric) yield promote(n0,n1)
    case LShiftOp|RShiftOp|UnsignedRShiftOp => for (n0 <- t0.unboxIntegral; _ <- t1.unboxIntegral) yield promote(n0)
    case LtOp|GtOp|LeOp|GeOp => for (n0 <- t0.unboxNumeric; n1 <- t1.unboxNumeric) yield BooleanType
    case EqOp|NeOp => ((t0,t1) match {
        case (BooleanType,_) if t1.unboxesToBoolean => true
        case (_,BooleanType) if t0.unboxesToBoolean => true
        case (_:PrimType,_) if t1.unboxesToNumeric => true
        case (_,_:PrimType) if t0.unboxesToNumeric => true
        case _ => castsTo(t0,t1) || castsTo(t1,t0)
      }) match {
        case true => Some(BooleanType)
        case false => None
      }
    case AndOp|XorOp|XorOp => (t0.unbox,t1.unbox) match {
      case (Some(BooleanType),Some(BooleanType)) => Some(BooleanType)
      case (Some(i0:IntegralType),Some(i1:IntegralType)) => Some(promote(i0,i1))
      case _ => None
    }
    case AndAndOp|OrOrOp => if (t0.unboxesToBoolean && t1.unboxesToBoolean) Some(BooleanType) else None
  }

  // TODO: Probably eliminate
  def unaryLegal(op: UnaryOp, t: Type) = unaryType(op,t).isDefined
  def binaryLegal(op: BinaryOp, t0: Type, t1: Type) = binaryType(op,t0,t1).isDefined

  // Substitute given tenv as two lists (assuming there is no parent environment to take into account)
  def substitute(vs: List[TypeVar], ts: List[RefType], t: Type): Type =
    if (vs.isEmpty) t
    else t.substitute((vs,ts.map(Some(_))).zipped.toMap)

  // Is lo a subtype (or subitem) of hi?
  def isSubtype(lo: Type, hi: Type): Boolean = lo==hi || (lo==NullType || lo.supers.exists(isSubtype(_,hi)))
  def isProperSubtype(lo: Type, hi: Type): Boolean = lo!=hi && (lo==NullType || lo.supers.exists(isSubtype(_,hi)))
  def isSubitem(lo: TypeItem, hi: TypeItem): Boolean = lo==hi || lo.superItems.exists(isSubitem(_,hi))

  // If lo <: hi, find the superclass of lo matching hi
  def subItemType(lo: Type, hi: ClassItem): Option[ClassType] =
    collectOne(supers(lo)){ case t:ClassType if t.item==hi => t }

  // Is a type throwable?
  def isThrowable(t: TypeItem): Boolean = isSubitem(t,ThrowableItem)

  // Is a type iterable or an array?  If so, what does it contain?
  def isIterable(i: Type): Option[Type] = i match {
    case ArrayType(t) => Some(t)
    case _ => subItemType(i,IterableItem) match {
      case Some(t:GenericType) => capture(t.item.tparams,t.args,Map.empty)._2 match {
        case List(t) => Some(t)
        case _ => throw new RuntimeException("arity mismatch")
      }
      case _ => None
    }
  }

  // Capture conversion for generic types with wildcards
  def capture(tparams: List[TypeVar], args: List[TypeArg], base: Tenv): (Tenv,List[RefType]) = {
    assert(tparams.size == args.size)
    // FreshVar contains public vars, but that's fine since its definition doesn't escape this function
    case class FreshVar(name: Name) extends TypeVar {
      def superItems = throw new RuntimeException("Should never happen")
      var lo: RefType = null
      var hi: RefType = null
    }
    var fills: List[Tenv => Unit] = Nil
    val vts = (tparams,args).zipped map { case (v,t) => (v,t match {
      case t:RefType => t
      case t:Wildcard => {
        val f = new FreshVar(v.name)
        fills = ((env: Tenv) => t match {
          case WildSub(t)   => f.lo =            v.lo.substitute(env)
                               f.hi = glb(List(t,v.hi.substitute(env)))
          case WildSuper(t) => f.lo = lub(List(t,v.lo.substitute(env)))
                               f.hi =            v.hi.substitute(env)
        }) :: fills
        f
      }
    })}
    val env = base ++ vts.toMap.mapValues(Some(_))
    for (f <- fills) f(env)
    (env,vts.unzip._2)
  }

  // Widening, narrowing, and widening-and-narrowing primitive conversions: 5.1.2, 5.1.3, 5.1.4
  def widensPrimTo(from: PrimType, to: PrimType): Boolean = (from,to) match {
    case (ByteType,ShortType|IntType|LongType|FloatType|DoubleType)
       | (ShortType|CharType,IntType|LongType|FloatType|DoubleType)
       | (IntType,                   LongType|FloatType|DoubleType)
       | (LongType,                           FloatType|DoubleType)
       | (FloatType,                                    DoubleType) => true
    case _ => false
  }
  def narrowsPrimTo(from: PrimType, to: PrimType): Boolean = (from,to) match {
    case (ShortType, ByteType|CharType)
       | (CharType,  ByteType|ShortType)
       | (IntType,   ByteType|ShortType|CharType)
       | (LongType,  ByteType|ShortType|CharType|IntType)
       | (FloatType, ByteType|ShortType|CharType|IntType|LongType)
       | (DoubleType,ByteType|ShortType|CharType|IntType|LongType|FloatType) => true
    case _ => false
  }
  def widensNarrowsPrimTo(from: PrimType, to: PrimType): Boolean = (from,to) match {
    case (ByteType,CharType) => true
    case _ => false
  }

  // Widening and narrowing reference conversions: 5.1.5, 5.1.6
  def widensRefTo(from: RefType, to: RefType): Boolean = isProperSubtype(from,to)
  def narrowsRefTo(from: RefType, to: RefType): Boolean = isProperSubtype(to,from) || (from!=to && ((from,to) match {
    case (f:ClassType,t:ClassType) => !f.isFinal && !t.isFinal && t.isSimple
    case (ArrayType(f:RefType),ArrayType(t:RefType)) => narrowsRefTo(f,t)
    case _ => false
  }))

  // Boxing and unboxing conversions: 5.1.7, 5.1.8
  def boxesTo(from: Type, to: RefType): Boolean = from match {
    case f: PrimType => f.box==to
    case NullType => to==NullType
    case _ => false
  }
  def unboxesTo(from: Type, to: PrimType): Boolean = from==to.box

  // Unchecked conversions: 5.1.9
  def uncheckedConvertsTo(from: Type, to: Type): Boolean = from==to.raw
  def widensRefUncheckedTo(from: RefType, to: RefType): Boolean = widensRefTo(from,to) || widensRefTo(from,to.raw)

  // Assignment contexts: 5.2
  def assignsTo(e: Exp, to: Type): Boolean =
    assignsTo(e.ty,to) || (to.unbox exists (constantFits(e,_)))
  def assignsTo(e: Option[Exp], to: Type): Boolean = e match {
    case None => to==VoidType
    case Some(e) => assignsTo(e,to)
  }
  def assignsTo(from: Type, to: Type): Boolean = {
    (from,to) match {
      case _ if from==to => true
      case (VoidType,_)|(_,VoidType) => false
      case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
      case (f: RefType, t: RefType) => widensRefUncheckedTo(f,t)
      case (f: PrimType, t: RefType) =>
        val fb = f.box
        fb == t || widensRefTo(fb,t)
      case (f: RefType, t: PrimType) => f.unbox match {
        case Some(fp) => widensPrimTo(fp,t)
        case None => false
      }
    }
  }

  // Invocation contexts: 5.3
  def strictInvokeContext(from: Type, to: Type): Boolean = (from,to) match {
    case _ if from==to => true
    case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
    case (f: RefType, t: RefType) => widensRefTo(f,t)
    case _ => false
  }
  def looseInvokeContext(from: Type, to: Type): Boolean = (from,to) match {
    case _ if from==to => true
    case (VoidType, _) => false
    case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
    case (f: RefType, t: RefType) => widensRefUncheckedTo(f,t)
    case (f: PrimType, t: RefType) =>
      val fb = f.box
      fb == t || widensRefTo(fb,t)
    case (f: RefType, t: PrimType) => f.unbox match {
      case Some(fp) => widensPrimTo(fp,t)
      case None => false
    }
  }

  // Whether from can be explicitly cast to to
  def castsTo(from: Type, to: Type): Boolean = from==to || ((from,to) match {
    case (VoidType,_) => false
    case (_,VoidType) => true
    case (f:PrimType,t:PrimType) => (f==BooleanType)==(t==BooleanType)
    case (f:RefType, t:PrimType) => (f.unbox,t) match {
      case (None,_) => f==ObjectType
      case (Some(ByteType),(ByteType|ShortType|IntType|LongType|FloatType|DoubleType))
         | (Some(ShortType),        (ShortType|IntType|LongType|FloatType|DoubleType))
         | (Some(CharType),          (CharType|IntType|LongType|FloatType|DoubleType))
         | (Some(IntType),                    (IntType|LongType|FloatType|DoubleType))
         | (Some(LongType),                           (LongType|FloatType|DoubleType))
         | (Some(FloatType),                                   (FloatType|DoubleType))
         | (Some(DoubleType),                                             DoubleType) => true
      case _ => false
    }
    case (f:PrimType,t:RefType) => t==ObjectType || t.unbox==Some(f)
    case (f:RefType,t:RefType) => isSubtype(f,t) || isSubtype(t,f)
  })

  // All supertypes of a reference type, including self
  def supers(t: RefType): Set[RefType] = {
    def loop(ss: Set[RefType], t: RefType): Set[RefType] =
      if (ss contains t) ss
      else t.supers.foldLeft(ss+t)(loop)
    loop(Set(),t)
  }
  def supers(t: Type): Set[RefType] = t match {
    case t:RefType => supers(t)
    case _ => Set()
  }
  def superItems(t: RefTypeItem): Set[RefTypeItem] = {
    def loop(ss: Set[RefTypeItem], t: RefTypeItem): Set[RefTypeItem] = {
      if (ss contains t) ss
      else t.superItems.foldLeft(ss+t)(loop)
    }
    t.superItems.foldLeft(Set(t))(loop)
  }

  // Least upper bounds: 4.10.4
  // TODO: Handle generics
  def lub(x: RefType, y: RefType): RefType = (x,y) match {
    case (x,NullType) => x
    case (NullType,y) => y
    case (x,y) =>
      val ss = supers(x) & supers(y)
      val mec = ss.filter(t => ss.forall(s => s==t || !isSubtype(s,t)))
      mec.toList match {
        case Nil => throw new RuntimeException("lub should never fail")
        case List(t) => t
        case _ => IntersectType(mec)
      }
  }
  // TODO: Implementing lub with fold is not correct
  def lub(xs: List[RefType]): RefType = xs match {
    case Nil => NullType
    case List(x) => x
    case x::xs => lub(x,lub(xs))
  }

  // Greatest lower bounds: 5.1.10
  def glb[A <: RefType](xs: List[A]): RefType = xs match {
    case Nil => ObjectType
    case List(x) => x
    case xs => IntersectType(xs.toSet)
  }

  // Combine left and right sides of a conditional expression
  // TODO: Handle poly expressions (admittedly, this doesn't even make sense with this signature)
  def condType(x: Type, y: Type): Type = {
    def pp(x: PrimType, y: PrimType): Type = (x,y) match {
      case (BooleanType,BooleanType) => BooleanType
      case (BooleanType,n:NumType) => lub(BooleanType.box,n.box)
      case (n:NumType,BooleanType) => lub(BooleanType.box,n.box)
      case (x:NumType,y:NumType) => promote(x,y)
    }
    def pr(x: PrimType, y: RefType): Type = (x,y.unbox) match {
      case (x,None) => lub(x.box,y)
      case (BooleanType,Some(BooleanType)) => BooleanType
      case (BooleanType,Some(y:NumType)) => lub(BooleanType.box,y.box)
      case (x:NumType,Some(BooleanType)) => lub(BooleanType.box,x.box)
      case (x:NumType,Some(y:NumType)) => promote(x,y)
    }
    (x,y) match {
      case (VoidType,_)|(_,VoidType) => VoidType
      case (x:PrimType,y:PrimType) => pp(x,y)
      case (x:PrimType,y:RefType) => pr(x,y)
      case (x:RefType,y:PrimType) => pr(y,x)
      case (x:RefType,y:RefType) => lub(x,y)
    }
  }

  // Combine a bunch of types into a single type (for array literal purposes)
  // TODO: This function is a bit strange, since array literals do not exist in Java.
  def condTypes(ts: List[Type]): Type = ts match {
    case Nil => ObjectType // TODO: Doesn't handle zero size primitive type arrays
    case List(x) => x
    case x :: xs => condType(x,condTypes(xs))
  }

  // Is t0 op= t1 valid?
  def assignOpType(op: Option[AssignOp], t0: Type, t1: Type): Option[Type] = op match {
    case Some(op) => binaryType(op,t0,t1) filter (castsTo(_,t0))
    case None => if (assignsTo(t1,t0)) Some(t0) else None
  }

  // Convenience functions for arrays
  def dimensions(t: Type): Int = t match {
    case ArrayType(t) => 1+dimensions(t)
    case _ => 0
  }
  def arrays(t: Type, dims: Int): Type = {
    if (dims == 0) t
    else arrays(ArrayType(t),dims-1)
  }
  @tailrec def hasDims(t: Type, dims: Int): Boolean = dims==0 || (t match {
    case ArrayType(t) => hasDims(t,dims-1)
    case _ => false
  })

  // Does type arguments match some type variables?  True if there *could be any* types that match both.
  def couldMatch(vs: List[TypeVar], args: List[TypeArg]): Boolean = {
    if (vs.size != args.size) false
    else {
      val (env,ts) = capture(vs,args,Map.empty)
      (vs,args).zipped forall { case (v,a) => {
        assert(v.lo == NullType)
        val hi = v.hi.substitute(env)
        a match {
          case t:RefType => isSubtype(t,hi)
          case WildSub(t) => isSubtype(t,hi) || isSubtype(hi,t)
          case WildSuper(t) => isSubtype(t,hi)
        }
      }}
    }
  }

  // Method resolution: generics and overloads, 15.12.2
  // Given a list of callables, find the most specific one along with its type parameters
  // TODO: Handle explicit type parameters, possibly by prefiltering fs outside of this function
  trait Signature {
    def tparams: List[TypeVar] // Inferable type parameters
    def params: List[Type]
  }

  // given argument types ts, which signatures are still usable? ts is allowed to be shorter than f.params,
  // all signatures still possible after matching the prefix are returned.
  def resolveOptions[F <: Signature](fs: List[F], ts: List[Type]): List[(F,List[RefType])] = {
    val n = ts.size
    // TODO: Handle access restrictions (public, private, protected) and scoping
    // TODO: Handle variable arity
    def potentiallyCompatible(f: F): Boolean = f.params.size >= n && {
      (f.params,ts).zipped forall {case (p,t) => true}} // TODO: Handle poly expression constraints
    def compatible(f: F, form: Inference.Form, context: (Type,Type) => Boolean): Option[List[RefType]] =
      if (f.tparams.isEmpty)
        if ((f.params.slice(0,ts.size),ts).zipped forall {case (p,t) => context(t,p)}) Some(Nil)
        else None
      else Inference.infer(f.tparams,f.params.slice(0,ts.size),ts)(form)
    def strictCompatible(f: F): Option[List[RefType]] = compatible(f,Inference.strictBounds, strictInvokeContext)
    def looseCompatible (f: F): Option[List[RefType]] = compatible(f,Inference.looseBounds , looseInvokeContext)
    // narrow down candidates
    val potential = fs filter potentiallyCompatible
    val applies = potential flatMap (f => strictCompatible(f).map((f,_)).toList) match {
      case Nil => potential flatMap (f =>  looseCompatible(f).map((f,_)).toList)
      case fs => fs
    }
    applies
  }

  // resolve an overloaded function
  def resolve[F <: Signature](fs: List[F], ts: List[Type]): Option[(F,List[RefType])] = {
    def mostSpecific(fs: List[(F,List[RefType])]): Option[(F,List[RefType])] = fs match {
      case Nil => None
      case List(f) => Some(f)
      case _ => notImplemented // TODO: more inference
    }
    // Pick function
    mostSpecific(resolveOptions(fs, ts))
  }
}
