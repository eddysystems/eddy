package tarski

// Language levels
object Levels {
  case class LangLevel(level: Int) extends AnyVal {
    def >=(x: LangLevel): Boolean = level >= x.level
  }

  val Java6 = LangLevel(6)
  val Java7 = LangLevel(7)
  val Java8 = LangLevel(8)

  def parseLevel(s: String): LangLevel =
    if      (s.startsWith("Java 6")) Java6
    else if (s.startsWith("Java 7")) Java7
    else if (s.startsWith("Java 8")) Java8
    else throw new RuntimeException(s"Unknown language level: $s")

  def parseLevelJava(s: String): Int = parseLevel(s).level
}
