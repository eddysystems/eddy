package utility

object Locations {
  // Helper functions, kept separate for use from Java
  def buildHelper(lo: Int, hi: Int): Long = lo | hi.toLong<<32
  def unionHelper(r0: Long, r1: Long): Long =
    buildHelper(Math.min(r0.toInt,r1.toInt),Math.max(r0>>>32,r1>>>32).toInt)
  def locatedHelper[A](x: A, r: Long): Located[A] = Located(x,new SRange(r))

  // Single source location
  class SLoc(val x: Int) extends AnyVal
  object SLoc {
    val unknown = new SLoc(Int.MinValue)
    def apply(x: Int): SLoc = new SLoc(x)
  }

  // Half open source ranges
  class SRange(val raw: Long) extends AnyVal {
    def lo: SLoc = new SLoc(raw.toInt)
    def hi: SLoc = new SLoc((raw>>>32).toInt)
    def union(y: SRange) = unionHelper(raw,y.raw)
    def contains(x: SLoc) = lo.x <= x.x && x.x < hi.x
    override def toString = s"SRange(${lo.x},${hi.x})"
  }
  object SRange {
    val unknown = SRange(Int.MinValue,Int.MaxValue)
    val empty   = SRange(Int.MaxValue,Int.MinValue)
    def apply(lo: Int, hi: Int): SRange = new SRange(buildHelper(lo,hi))
  }

  trait HasRange {
    def r: SRange
  }
  trait SetRange[+A] {
    def setR(r: SRange): A
  }

  case class Located[A](x: A, r: SRange) extends HasRange {
    def map[B](f: A => B): Located[B] = Located(f(x),r)

    // For Java use
    def raw: Long = r.raw
    def rawLo: Int = r.lo.x
    def rawHi: Int = r.hi.x
  }
}
