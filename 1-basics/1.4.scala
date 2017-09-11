import org.scalatest._

import scala.io.Source

val sourceURL = "https://cryptopals.com/static/challenge-data/4.txt"
val input = Source.fromURL(sourceURL)
val messages = input.getLines
val decodedMessages = messages.map(Decoder.crack(_))
val mostSensibleDecodedMessage = decodedMessages.maxBy(Gibberishness.weight(_))


new FlatSpec with Matchers {
  it should "pick the most sensible message after decodings" in {
    mostSensibleDecodedMessage should equal("Now that the party is jumping\n")
  }
}.execute
