import org.scalatest._

import io.Source
import java.util.Base64

object Gibberishness {

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

    def weight(input: String): Double = {
      input.map((c) => Character.toUpperCase(c)).map((c) => letterFrequencies.getOrElse(c, 0.0)).sum
    }
}

object Decoder {
  def crackedKey(encodedMessage: List[Byte]): Byte = {
    val candidates = (0 to 255) map { c =>
      val key = Array.fill(encodedMessage.length / 2)(c.toByte)
      val result = (encodedMessage, key).zipped.foldLeft("") { case (acc, (messageRawByte, keyByte)) =>
        acc ++ (messageRawByte ^ keyByte).toChar.toString
      }
      (result, c)
    }
    candidates.maxBy { case (result, key)  => Gibberishness.weight(result) }._2.toByte
  }
}

def decodeBase64(encoded: String): Array[Byte] = {
  Base64.getDecoder().decode(encoded)
}

def numberOf1Bits(input: Byte): Int = {
  var n = input
  var result = 0
  while (n != 0) {
    result = result + (n & 1)
    n = (n >> 1).toByte
  }
  result
}

def hammingDistanceBytes(left: Byte, right: Byte): Int = {
  numberOf1Bits((left ^ right).toByte)
}

def hammingDistance(left: Array[Byte], right: Array[Byte]): Int = {
  left.zip(right).foldLeft(0) { case (acc, (leftByte, rightByte)) =>
    acc + hammingDistanceBytes(leftByte, rightByte)
  }
}

def xor(operandOne: Array[Byte], operandTwo: Array[Byte]): Array[Byte] = {
  val result = (operandOne, operandTwo).zipped.foldRight(List[Byte]()) { case ((byteOne, byteTwo), acc) =>
    (byteOne ^ byteTwo).toByte :: acc
  }
  result.toArray
}

def repeatToSize(input: List[Byte], size: Int): Array[Byte] = {
  Stream.continually(input.toStream).flatten.take(size).toArray
}

new FlatSpec with Matchers {
  it should "break a repeating-key xor" in {
    val sourceURL = "https://cryptopals.com/static/challenge-data/6.txt"
    val input = decodeBase64(Source.fromURL(sourceURL).getLines.mkString)
    val hammingDistancesToKeySizes = (2 to 40).map { keySize =>
      val blocks = input.grouped(keySize)
      val blockPairs: List[List[Array[Byte]]] = blocks.toList.combinations(2).toList
      val hammingDistances = blockPairs.map { (pair: List[Array[Byte]]) =>
        hammingDistance(pair(0), pair(1)) / pair(0).size.toDouble
      }.toList
      (hammingDistances.sum / blockPairs.length, keySize)
    }
    val keySize = hammingDistancesToKeySizes.minBy(_._1)._2
    val inputTruncatedToMultipleOfKeySize = input.take((input.length / keySize) * keySize)
    val blocks = inputTruncatedToMultipleOfKeySize.grouped(keySize)
    val transposedBlocks = blocks.toList.transpose
    val key = (transposedBlocks.map { transposedBlock =>
      Decoder.crackedKey(transposedBlock)
    })
    val repeatingKey = repeatToSize(key, input.size)
    val actual = xor(input, repeatingKey).map(_.toChar).mkString
    val firstLineOfExpected =  "I'm back and I'm ringin' the bell "
    actual.lines.next should equal(firstLineOfExpected)
  }
}.execute
