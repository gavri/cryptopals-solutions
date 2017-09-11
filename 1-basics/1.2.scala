import org.scalatest._

import java.util.Base64

def decodeHex(hex: String): BigInt = {
  BigInt(hex, 16)
}

def hexXor(operandOne: String, operandTwo: String): String = {
  val operandOneValue = decodeHex(operandOne)
  val operandTwoValue = decodeHex(operandTwo)
  val xoredValue = operandOneValue ^ operandTwoValue
  xoredValue.toString(16)
}

val operandOne = "1c0111001f010100061a024b53535009181c"
val operandTwo = "686974207468652062756c6c277320657965"

new FlatSpec with Matchers {
  it should "xor hex values" in {
    val expected = "746865206b696420646f6e277420706c6179"
    val actual = hexXor(operandOne, operandTwo)
    actual should equal(expected)
  }
}.execute
