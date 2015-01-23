package utility

object Locations {
  // Helper functions, kept separate for use from Java
  def buildHelper(lo: Int, hi: Int): Long = lo&0xffffffffL | hi.toLong<<32
  def unionHelper(r0: Long, r1: Long): Long =
    buildHelper(Math.min(r0.toInt,r1.toInt),Math.max(r0>>>32,r1>>>32).toInt)
  def locatedHelper[A](x: A, r: Long): Loc[A] = Loc(x,new SRange(r))

  // Single source location
  class SLoc(val raw: Int) extends AnyVal {
    def +(d: Int) = SLoc(raw+d)
    def -(d: Int) = SLoc(raw-d)
    override def toString = if (this == SLoc.unknown) "SLoc.unknown" else s"SLoc($raw)"
  }
  object SLoc {
    val unknown = new SLoc(Int.MaxValue)
    def apply(x: Int): SLoc = new SLoc(x)
  }

  // Half open source ranges
  class SRange(val raw: Long) extends AnyVal {
    def r = this
    def lo: SLoc = new SLoc(raw.toInt)
    def hi: SLoc = new SLoc((raw>>>32).toInt)
    def before: SRange = if (normal) SRange(lo,lo) else this
    def after: SRange  = if (normal) SRange(hi,hi) else this

    def union(y: SRange): SRange = new SRange(unionHelper(raw,y.raw))
    def union(y: Option[SRange]): SRange = y match {
      case None => this
      case Some(y) => this union y
    }
    def unionR[A <: HasRange with AnyRef](y: Option[A]): SRange = y match {
      case None => this
      case Some(y) => this union y.r
    }
    def unionR[A <: HasRange](y: List[A]): SRange = y match {
      case Nil => this
      case List(y) => this union y.r
      case y::ys => this union y.r union ys.last.r
    }

    def contains(x: SLoc): Boolean = lo.raw <= x.raw && x.raw < hi.raw
    private def known: Boolean = this != SRange.unknown
    private def normal: Boolean = this != SRange.unknown && this != SRange.empty
    override def toString = if (!known) "SRange.unknown"
                            else if (this == SRange.empty) "SRange.empty"
                            else s"SRange(${lo.raw},${hi.raw})"
  }
  object SRange {
    val unknown = SRange(SLoc(Int.MinValue),SLoc(Int.MaxValue))
    val empty   = SRange(SLoc(Int.MaxValue),SLoc(Int.MinValue))
    def apply(lo: SLoc, hi: SLoc): SRange = new SRange(buildHelper(lo.raw,hi.raw))
  }

  // Left and right locations for a grouping
  case class SGroup(raw: Long) extends AnyVal {
    def lr = new SRange(raw)
    def l = if (known) SRange(lr.lo,lr.lo+1) else SRange.unknown
    def r = if (known) SRange(lr.hi-1,lr.hi) else SRange.unknown
    private def known: Boolean = this != SGroup.unknown
    override def toString = if (!known) "SGroup.unknown"
                            else s"SGroup(${lr.lo.raw},${lr.hi.raw})"
  }
  case object SGroup {
    def apply(l: SRange, r: SRange) = new SGroup(l.union(r).raw)
    def approx(lr: SRange) = new SGroup(lr.raw)
    val unknown = approx(SRange.unknown)
  }

  trait HasRange {
    def r: SRange
  }

  // We will have many case classes that include ranges, often more than one.
  // Our convention is that the range of a field follows the field and has a name ending with "r".
  case class Loc[+A](x: A, r: SRange) extends HasRange {
    def map[B](f: A => B): Loc[B] = Loc(f(x),r)

    // For Java use
    def raw: Long = r.raw
    def rawLo: Int = r.lo.raw
    def rawHi: Int = r.hi.raw
  }
  case class Grouped[+A](x: A, a: SGroup) extends HasRange {
    def r = a.lr
    def map[B](f: A => B) = Grouped(f(x),a)
  }
}
