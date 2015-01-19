package utility

object Locations {
  // Helper functions, kept separate for use from Java
  def betweenHelper(r0: Long, r1: Long): Long =
    r0.toInt | (r1>>>32<<32)
  def buildHelper(lo: Int, hi: Int): Long = lo | hi.toLong<<32
  def locatedHelper[A](x: A, r: Long): Located[A] = Located(x,new SRange(r))

  // Single source location
  class SLoc(val x: Int) extends AnyVal
  object SLoc {
    val unknown = new SLoc(-1)
    def apply(x: Int): SLoc = new SLoc(x)
  }

  // Inclusive source range
  class SRange(val raw: Long) extends AnyVal {
    def lo: SLoc = new SLoc(raw.toInt)
    def hi: SLoc = new SLoc((raw>>>32).toInt)
    def between(y: SRange) = betweenHelper(raw,y.raw)
    def contains(x: SLoc) = lo.x <= x.x && x.x <= hi.x
    override def toString = s"SRange(${lo.x},${hi.x})"
  }
  object SRange {
    val unknown = new SRange(-1)
    def apply(lo: Int, hi: Int): SRange = new SRange(buildHelper(lo,hi))
  }

  trait HasRange {
    def r: SRange
  }

  case class Located[A](x: A, r: SRange) extends HasRange {
    def map[B](f: A => B): Located[B] = Located(f(x),r)
    def raw: Long = r.raw // For Java use
  }
}
