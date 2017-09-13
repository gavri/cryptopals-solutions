import org.scalatest._

import io.Source
import scala.collection.mutable.Map

def numberOfIdenticalBlocksOfSize(text: String, size: Int) = {
  var substringFrequencies = Map[String, Int]()
  (0 until text.size - size).foreach { index =>
    val substring = text.slice(index, index + size)
    substringFrequencies(substring) = substringFrequencies.getOrElse(substring, 0) + 1
  }
  substringFrequencies.filter{ case (_, v) => v > 2 }.values.sum
}

def numberOfIdenticalBlocks(text: String) = {
  val sizesInBytes = 6 to 10
  val sizesInNibbles = sizesInBytes.map(_ * 2)
  sizesInNibbles.map { size =>
    numberOfIdenticalBlocksOfSize(text, size)
  }.sum
}


new FlatSpec with Matchers {
  it should "decrypt AES ECB 128 encrypted message" in {
    val sourceURL = "https://cryptopals.com/static/challenge-data/8.txt"
    val lines = Source.fromURL(sourceURL).getLines
    val actual = lines.maxBy(numberOfIdenticalBlocks(_))
    val expected = "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a"
    actual should equal(expected)
  }
}.execute
