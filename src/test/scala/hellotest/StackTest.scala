package hellotest

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.Queue

class StackTest extends AnyFunSuite with CloudSimple {

  // Helper function to simulate doOutput functionality, capturing output for tests
  var capturedOutput: Seq[(String, Long)] = Seq.empty

  override def doOutput(cloud: Cloud): Boolean = {
    capturedOutput = cloud._2
    true
  }

  // "generate correct word frequencies" test
  test("generate correct word frequencies") {
    val inputWords = Iterator("hello", "world", "hello")
    computeWordClouds(inputWords, howMany = 2, lastNWords = 3, everyKSteps = 1, minFrequency = 1)

    val expectedOutput = Seq(("hello", 2), ("world", 1))
    assert(capturedOutput == expectedOutput)
  }

  // "respect the window size" test
  test("respect the window size") {
    val inputWords = Iterator("Scala", "is", "great", "Scala", "testing")
    computeWordClouds(inputWords, howMany = 5, lastNWords = 3, everyKSteps = 1, minFrequency = 1)

    val expectedOutput = Seq(("Scala", 1), ("testing", 1), ("great", 1))  // Only last 3 words in window
    assert(capturedOutput == expectedOutput)
  }

  // "handle an empty input" test
  test("handle an empty input") {
    val inputWords = Iterator.empty[String]
    capturedOutput = Seq.empty
    computeWordClouds(inputWords, howMany = 5, lastNWords = 3, everyKSteps = 1, minFrequency = 1)
    assert(capturedOutput.isEmpty)
  }

  // "respect the cloud size limit" test
  test("respect the cloud size limit") {
    val inputWords = Iterator("a", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")
    computeWordClouds(inputWords, howMany = 3, lastNWords = 10, everyKSteps = 1, minFrequency = 1)

    assert(capturedOutput.size <= 3)
  }

  // "filter words by minimum frequency" test
  test("filter words by minimum frequency") {
    val inputWords = Iterator("repeat", "word", "repeat", "word", "single")
    computeWordClouds(inputWords, howMany = 5, lastNWords = 5, everyKSteps = 1, minFrequency = 2)

    val expectedOutput = Seq(("repeat", 2), ("word", 2))
    assert(capturedOutput == expectedOutput)
  }
}
