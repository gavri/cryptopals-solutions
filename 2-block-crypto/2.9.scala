import org.scalatest._

def padToLength(input: String, length: Int) = {
  val lengthOfPadding = length - input.size
  val padding = new String(Array.fill(lengthOfPadding)(lengthOfPadding.toByte))
  input + padding
}

new FlatSpec with Matchers {
  it should "should pad any text to a specified length" in {
    val input = "YELLOW SUBMARINE"
    val expected = "YELLOW SUBMARINE\u0004\u0004\u0004\u0004"
    val actual = padToLength("YELLOW SUBMARINE", 20)
    actual.lines.next should equal(expected)
  }
}.execute
