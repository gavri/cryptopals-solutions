import java.util.Base64

object Main extends App {

  //From https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
  //I snuck the space character in after N with a made up frequency so it's close to
  //"ETAOIN SHRDLU"
  def letterFrequencies = Map(
    'E' -> 12.02,
    'T' -> 9.10,
    'A' -> 8.12,
    'O' -> 7.68,
    'I' -> 7.31,
    'N' -> 6.95,
    ' ' -> 6.50,
    'S' -> 6.28,
    'R' -> 6.02,
    'H' -> 5.92,
    'D' -> 4.32
  )

  def weightOfNonGibberishness(input: String): Double = {
    input.map((c) => Character.toUpperCase(c)).map((c) => letterFrequencies.getOrElse(c, 0.0)).sum
  }

  def hexToBytes(hex: String): Array[Byte] = {
    val result = hex.grouped(2).foldLeft(List[Byte]()) { (acc, byteInHex) =>
      Integer.parseInt(byteInHex, 16).toByte :: acc
    }
    result.reverse.toArray
  }

  val message = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  val messageRaw = hexToBytes(message)
  val candidates = (0 to 255) map { c =>
    val key = Array.fill(message.length / 2)(c.toByte)
    val result = (messageRaw, key).zipped.foldLeft("") { case (acc, (messageRawByte, keyByte)) =>
      acc ++ (messageRawByte ^ keyByte).toChar.toString
    }
    result
  }
  println(candidates.maxBy((x) => weightOfNonGibberishness(x)))
}
