import java.util.Base64

object Main extends App {

  def decodeHex(hex: String): Array[Byte] = {
    BigInt(hex, 16).toByteArray
  }

  def encodeBase64(bytes: Array[Byte]): String = {
    Base64.getEncoder().encodeToString(bytes)
  }

  val hexToBase64 = (decodeHex _).andThen(encodeBase64 _)

  val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  val expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

  val actual = hexToBase64(input)

  assert(expected == actual)
}
