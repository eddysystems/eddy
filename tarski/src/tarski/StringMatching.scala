package tarski

import scala.collection.mutable.ArrayBuffer

/**
 * Created by martin on 11.12.14.
 */
object StringMatching {

  val repeatCost = 1.0f
  val doubleTypeMinDist = 1.0f
  val doubleTypeMin = 1.0f
  val doubleTypeMax = 4.0f
  val deleteCostConst = 1.0f
  val insertAddShiftStroke = 2.0f // a spurious isolated capital letter
  val insertOmitShiftStroke = 2.0f // a lowercase letter that should've been uppercase in the middle of two uppercase letters
  val replaceAddShiftStroke = 0.5f // a spurious isolated capital letter
  val replaceOmitShiftStroke = 0.5f // a lowercase letter that should've been uppercase in the middle of two uppercase letters
  val extendShift = .3f // shift key held down too long or let go too quickly
  val swapCostConst = .3f // the cost of swapping two adjacent letters
  val minSwapCost = swapCostConst // the minimum cost of a swap operation (for lower bounds)
  val minInsertCost = doubleTypeCost(1.0f)

  // not considering case
  def charDistance(c1: Char, c2: Char): Float = {
    if (c1.toLower==c2.toLower)
      0.0f
    else
      1.0f
  }

  // converts a character distance to the cost of it having been typed by hitting two keys at once
  def doubleTypeCost(cd: Float): Float = {
    if (cd < doubleTypeMinDist)
      doubleTypeMin
    else
      math.min(cd * cd, doubleTypeMax)
  }

  def insertShiftCost(before: Boolean, c: Boolean, after: Boolean): Float = {
    if (before == after && before == false && c == true) // bXa
      insertAddShiftStroke
    else if (before == after && before == true && c == false) // BxA
      insertOmitShiftStroke
    else if (after != before && c == true) // BXa, bXA
      extendShift
    else
      0.0f
  }

  def replaceShiftCost(before: Boolean, ct: Boolean, cm: Boolean, after: Boolean): Float = {
    if (cm == ct)
      0.0f
    else if (before == ct || ct == after)
      extendShift
    else if (ct)
      replaceAddShiftStroke
    else
      replaceOmitShiftStroke
  }

  def insertCost(meant: CharSequence, i: Int, typed: CharSequence, j: Int): Float = {
    // accidentally hit a key next to the key before or after?
    val ca = meant.charAt(i) // the key we mean to press
    val ci = typed.charAt(j) // this is the key we're inserting (accidentally)

    if (j == 0) {
      doubleTypeCost(math.max(1.0f,charDistance(ca, ci)))
    } else {
      val cb = typed.charAt(j-1) // the key we just pressed
      insertShiftCost(cb.isUpper, ci.isUpper, ca.isUpper) + doubleTypeCost(math.min(math.max(charDistance(cb, ci),1.0f), math.max(1.0f,charDistance(ca, ci))))
    }
  }

  def deleteCost(meant: CharSequence, i: Int, typed: CharSequence, j: Int): Float = {
    deleteCostConst
  }

  def replaceCost(meant: CharSequence, i: Int, typed: CharSequence, j: Int): Float = {
    val cm = meant.charAt(i)
    val ct = typed.charAt(j)
    val cn = if (i+1 < meant.length) meant.charAt(i+1) else cm
    val cp = if (j-1 >= 0) typed.charAt(j-1) else cn
    replaceShiftCost(cp.isUpper, ct.isUpper, cm.isUpper, cn.isUpper) + charDistance(cm,ct)
  }

  def swapCost(meant: CharSequence, i: Int, typed: CharSequence, j: Int): Float = {
    // cost for swapping i, i+1 to j, j+1
    // what would the cost be if we hadn't swapped?
    val unswapped: CharSequence = meant.subSequence(0,i) + meant.charAt(i+1).toString + meant.charAt(i).toString + meant.subSequence(i+2,meant.length)
    swapCostConst + replaceCost(unswapped, i, typed, j) + replaceCost(unswapped, i+1, typed, j+1)
  }

  def levenshteinDistance(meant: CharSequence, typed: CharSequence): Float =
    levenshteinDistance(meant, typed,
                        insertCost,
                        deleteCost,
                        replaceCost,
                        swapCost)

  // user intends to write meant, how likely is it he wrote to typed? Return is a cost: lower is more likely
  def levenshteinDistance(meant: CharSequence, typed: CharSequence,
                          insertCost: (CharSequence, Int, CharSequence, Int) => Float,
                          deleteCost: (CharSequence, Int, CharSequence, Int) => Float,
                          replaceCost: (CharSequence, Int, CharSequence, Int) => Float,
                          swapCost: (CharSequence, Int, CharSequence, Int) => Float): Float = {
    // d(i,j) is cost to obtain the first i character of meant having used the first j characters of typed
    val d = ArrayBuffer.fill(meant.length + 1, typed.length + 1)(0.0f)

    // fill first column (moving down equals deletion of a character in meant
    for (i <- 1 to meant.length) {
      d(i)(0) = d(i-1)(0) + deleteCost(meant, i-1, "", 0)
    }

    // fill first row (moving right equals inserting a character into typed)
    for (i <- 1 to typed.length) {
      d(0)(i) = d(0)(i-1) + insertCost(meant, 0, typed, i-1)
    }

    // fill the rest
    for (i <- 1 to meant.length;
         j <- 1 to typed.length) {
      // we're mentally at character i of what we intended to write, and we have already written j characters
      d(i)(j) = List(d(i-1)(j) + deleteCost(meant, i-1, typed, j-1), // omit a character of what we intended to write
                     d(i)(j-1) + insertCost(meant, i-1, typed, j-1), // insert a character typed[j-1] accidentally (without advancing our mental state of where we are with typing)
                     d(i-1)(j-1) + replaceCost(meant, i-1, typed, j-1) // type a character (maybe getting it wrong)
                     ).min
      // swapped two characters?
      if (i > 1 && j > 1)
        d(i)(j) = List(d(i)(j),
                       d(i-2)(j-2) + swapCost(meant, i-2, typed, j-2)
                       ).min

      // TODO: three subsequent replace actions are cheaper especially if the letters scrambled are on opposite
      // sides of the keyboard (synchronization between hands is hard)
    }

    d(meant.length)(typed.length)
  }

  def editDistance[A](meant: Seq[A], typed: Seq[A],
                      insertCost: (Seq[A], Int, Seq[A], Int) => Float,
                      deleteCost: (Seq[A], Int, Seq[A], Int) => Float,
                      replaceCost: (Seq[A], Int, Seq[A], Int) => Float,
                      swapCost: (Seq[A], Int, Seq[A], Int) => Float): Float = {
    // d(i,j) is cost to obtain the first i character of s having used the first j characters of t
    val d = ArrayBuffer.fill(meant.length + 1, typed.length + 1)(0.0f)

    // fill first column (moving down equals deletion of a character in from
    for (i <- 1 to meant.length) {
      d(i)(0) = d(i-1)(0) + deleteCost(meant, i-1, Nil, 0)
    }

    // fill first row (moving right equals inserting a character into to
    for (i <- 1 to typed.length) {
      d(0)(i) = d(0)(i-1) + insertCost(meant, 0, typed, i-1)
    }

    // fill the rest
    for (i <- 1 to meant.length;
         j <- 1 to typed.length) {
      // we're mentally at character i of what we intended to write, and we have already written j characters
      d(i)(j) = List(d(i-1)(j) + deleteCost(meant, i-1, typed, j-1), // omit a character of what we intended to write
                     d(i)(j-1) + insertCost(meant, i-1, typed, j-1), // insert a character typed[j-1] accidentally (without advancing our mental state of where we are with typing)
                     d(i-1)(j-1) + replaceCost(meant, i-1, typed, j-1) // type a character (maybe getting it wrong)
                     ).min
      // swapped two characters?
      if (i > 1 && j > 1)
        d(i)(j) = List(d(i)(j),
                       d(i-2)(j-2) + swapCost(meant, i-2, typed, j-2)
                       ).min

      // TODO: three subsequent replace actions are cheaper especially if the letters scrambled are on opposite
      // sides of the keyboard (synchronization between hands is hard)
    }

    d(meant.length)(typed.length)
  }

  trait IncrementalDistance {
    // the position in the meant string we're at
    def i: Int
    // the currently meant string
    def current: String
    // the prefixDistance of current minus the last character
    def prefixDistance: IncrementalDistance

    // row i of the distance matrix
    def d(i: Int): Float

    // the minimum distance that's still possible to achieve by appending characters
    def min: Float

    // the current distance (assuming nothing more is added
    def distance: Float
  }

  class IncrementalLevenshteinBound(typed: String, override val prefixDistance: IncrementalDistance, c: Char) extends IncrementalDistance {
    // we are at position i in meant
    val i: Int = prefixDistance.i + 1
    val current: String = prefixDistance.current + c

    // we're creating row i
    val _d: Array[Float] = new Array(typed.length+1)
    def d(i: Int): Float = _d(i)

    _d(0) = prefixDistance.d(0) + deleteCost(current, i-1, "", 0)
    for (j <- 1 to typed.length) {
      _d(j) = List(prefixDistance.d(j) + deleteCost(current, i-1, typed, j-1), // omit a character of what we intended to write
                  d(j-1) + insertCost(current, i-1, typed, j-1), // insert a character typed[j-1] accidentally (without advancing our mental state of where we are with typing)
                  prefixDistance.d(j-1) + replaceCost(current, i-1, typed, j-1) // type a character (maybe getting it wrong)
                  ).min
      if (j > 1 && i > 1)
        _d(j) = List(d(j),
                     prefixDistance.prefixDistance.d(j-2) + swapCost(current, i-2, typed, j-2)
                     ).min
    }

    // the minimum distance that's still possible to achieve by appending more characters
    // (this assumes that the minimum replaceCost is 0, so there is a potential zero-cost path from
    // all points to the far right of the matrix)
    def min: Float = if (_d.length > 2)
                       List(_d.min, ((0 until _d.length-2) map prefixDistance.d).min + minSwapCost).min
                     else
                       _d.min

    // the current distance if no more characters are appended
    def distance: Float = _d.last
  }

  object EmptyIncrementalLevenshteinBound extends IncrementalDistance {
    val i = 0
    val current = ""
    def prefixDistance = throw new RuntimeException("Accessing prefix of empty string distance")
    def d(i: Int): Float = i * minInsertCost // we use the lower bound here because we cannot look ahead
    def min: Float = 0
    def distance: Float = 0
  }
}
