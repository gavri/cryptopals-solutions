import scala.io.Source

object Main extends App {
  val sourceURL = "https://cryptopals.com/static/challenge-data/4.txt"
  val input = Source.fromURL(sourceURL)
  val messages = input.getLines
  val decodedMessages = messages.map(Decoder.crack(_))
  val mostSensibleDecodedMessage = decodedMessages.maxBy(Gibberishness.weight(_))
  println(mostSensibleDecodedMessage)
}
