package hellotest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.Queue
import scala.collection.immutable.Map // Use immutable Map to match the OutputSink trait

class MainTest extends AnyFlatSpec with Matchers {

  // Mock OutputSink for testing
  class MockOutputSink extends OutputSink {
    var printedWordCloud: Map[String, Int] = Map()

    // Ensure the method matches the trait
    override def print(wordCloud: Map[String, Int]): Unit = {
      printedWordCloud = wordCloud
    }
  }

  "wordcloud" should "generate correct word frequencies" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("hello", "world", "hello", "today", "world", "hello", "scala", "programming")
    val cloudSize = 2
    val minLength = 3
    val windowSize = 5
    val minFrequency = 2

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check the printed word cloud
    mockOutput.printedWordCloud should contain("hello")
    mockOutput.printedWordCloud should contain("world")
    mockOutput.printedWordCloud shouldNot contain("today")
    mockOutput.printedWordCloud shouldNot contain("scala")
  }

  it should "respect minimum word length" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("hi", "hello", "there", "hi", "world")
    val cloudSize = 2
    val minLength = 4 // Set minimum length to 4
    val windowSize = 5
    val minFrequency = 1

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check the printed word cloud
    mockOutput.printedWordCloud should contain("hello")
    mockOutput.printedWordCloud should contain("there")
    mockOutput.printedWordCloud shouldNot contain("hi")
    mockOutput.printedWordCloud shouldNot contain("world")
  }

  it should "handle an empty input" in {
    val mockOutput = new MockOutputSink

    val words = Iterator.empty
    val cloudSize = 5
    val minLength = 3
    val windowSize = 10
    val minFrequency = 1

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check that the printed word cloud is empty
    mockOutput.printedWordCloud shouldBe empty
  }

  it should "respect the window size" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("apple", "banana", "apple", "orange", "banana", "banana", "grape")
    val cloudSize = 3
    val minLength = 3 // Set minimum length to 3
    val windowSize = 4 // Set window size to 4
    val minFrequency = 2

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check the printed word cloud
    mockOutput.printedWordCloud should contain("banana") // Should be counted
    mockOutput.printedWordCloud shouldNot contain("apple") // Out of the window size
    mockOutput.printedWordCloud shouldNot contain("orange") // Out of the window size
    mockOutput.printedWordCloud shouldNot contain("grape") // Not enough frequency
  }
}
