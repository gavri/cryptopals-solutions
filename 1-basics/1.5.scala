import org.scalatest._

object XorBytes {
  def xor(operandOne: Array[Byte], operandTwo: Array[Byte]): Array[Byte] = {
    val result = (operandOne, operandTwo).zipped.foldRight(List[Byte]()) { case ((byteOne, byteTwo), acc) =>
      (byteOne ^ byteTwo).toByte :: acc
    }
    result.toArray
  }
}

def bytesToHex(bytes: Array[Byte]): String =  {
  bytes.map("%02X" format _).mkString
}

new FlatSpec with Matchers {
  it should "encrypt by repeating a xor key" in {
    val message = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val messageBytes = message.getBytes
    val key = ("ICE" * Math.ceil(message.length / 3.0).toInt).take(message.length)
    val keyBytes = key.getBytes
    val expected = "0B3637272A2B2E63622C2E69692A23693A2A3C6324202D623D63343C2A26226324272765272A282B2F20430A652E2C652A3124333A653E2B2027630C692B20283165286326302E27282F"
    val actual = bytesToHex(XorBytes.xor(messageBytes, keyBytes))
    actual should equal(expected)
  }
}.execute

