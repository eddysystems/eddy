package tarski

import scala.collection.mutable.ArrayBuffer

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

  // user intends to write meant, how likely is it he wrote to typed? Return is a cost: lower is more likely
  def levenshteinDistance(meant: CharSequence, typed: CharSequence,
                          insertCost: (CharSequence, Int, CharSequence, Int) => Float = insertCost,
                          deleteCost: (CharSequence, Int, CharSequence, Int) => Float = deleteCost,
                          replaceCost: (CharSequence, Int, CharSequence, Int) => Float = replaceCost): Float = {
    // d(i,j) is cost to obtain the first i character of s having used the first j characters of t
    val d = ArrayBuffer.fill(meant.length + 1, typed.length + 1)(0.0f)

    // fill first column (moving down equals deletion of a character in from
    for (i <- 1 to meant.length) {
      d(i)(0) = d(i-1)(0) + deleteCost(meant, i-1, "", 0)
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

      // TODO: two subsequent replace actions (which swap two characters), or three subsequent replace actions
      // (which permute three characters) should have much lower cost, especially if the letters scrambled are on opposite
      // sides of the keyboard (synchronization between hands is hard)
    }

    d(meant.length)(typed.length)
  }

}
