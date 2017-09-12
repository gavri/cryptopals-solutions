import org.scalatest._

import io.Source
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

val sourceURL = "https://cryptopals.com/static/challenge-data/7.txt"
val input = Base64.getDecoder().decode(Source.fromURL(sourceURL).getLines.mkString)
val cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
val secretKey = new SecretKeySpec("YELLOW SUBMARINE".getBytes, "AES");
cipher.init(Cipher.DECRYPT_MODE, secretKey)
new FlatSpec with Matchers {
  it should "decrypt AES ECB 128 encrypted message" in {
    val actual = cipher.doFinal(input).map(_.toChar).mkString
    val firstLineOfExpected = "I'm back and I'm ringin' the bell "
    actual.lines.next should equal(firstLineOfExpected)
  }
}.execute
