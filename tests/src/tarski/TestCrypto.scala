package tarski

import tarski.Crypto._
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestCrypto {
  @Test def key(): Unit = {
    val key: String = randomKey()
    assertEquals(keyBits/4,key.size)
    val bytes: Array[Byte] = hexToBytes(key)
    assert(bytes.size==keyBits/8)
    assertEquals(key,bytesToHex(bytes))
  }

  @Test def hexBytes(): Unit = {
    val hex = "6edef75b596f5c0004bb8103a1f1fddd237e5268dfb61b1d7a3e1a88"
    assertEquals(hex,bytesToHex(hexToBytes(hex)))
  }

  @Test def bytesHex(): Unit = {
    var i = 7
    val bytes = Array.fill(keyBits/8)({ i += 1; i.toByte })
    assertEquals(bytes,hexToBytes(bytesToHex(bytes)))
  }
}
