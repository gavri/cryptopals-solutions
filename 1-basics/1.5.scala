object Main extends App {
  def xor(operandOne: Array[Byte], operandTwo: Array[Byte]): Array[Byte] = {
    val result = (operandOne, operandTwo).zipped.foldRight(List[Byte]()) { case ((byteOne, byteTwo), acc) =>
      (byteOne ^ byteTwo).toByte :: acc
    }
    result.toArray
  }

  def bytesToHex(bytes: Array[Byte]): String =  {
    bytes.map("%02X" format _).mkString
  }

  val message = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  val messageBytes = message.getBytes
  val key = ("ICE" * Math.ceil(message.length / 3.0).toInt).take(message.length)
  val keyBytes = key.getBytes
  println(bytesToHex(xor(messageBytes, keyBytes)))
}
