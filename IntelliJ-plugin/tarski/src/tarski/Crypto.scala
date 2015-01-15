package tarski

import java.security.SecureRandom

object Crypto {
  // Size of random keys in bits
  val keyBits = 224

  // Cryptographic random keys
  def randomKey(): String = {
    val bytes = new Array[Byte](keyBits/8)
    (new SecureRandom).nextBytes(bytes)
    bytesToHex(bytes)
  }

  // Conversion between byte arrays and hex strings
  def bytesToHex(bs: Array[Byte]): String =
    bs.map("%02x" format _).mkString
  def hexToBytes(hs: String): Array[Byte] =
    hs.sliding(2,2).map(java.lang.Integer.parseInt(_,16).toByte).toArray
}
