package hellotest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.Map // Use immutable Map to match the OutputSink trait
import hellotest.{Main, OutputSink}

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
    val windowSize = 8
    val minFrequency = 2

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Expected frequencies
    val expectedFrequencies = Map("world" -> 2, "hello" -> 3)

    // Check the printed word cloud for correct frequencies
    mockOutput.printedWordCloud.toMap shouldEqual expectedFrequencies


    // Check the printed word cloud
    mockOutput.printedWordCloud.keys should contain allOf ("hello", "world")
    mockOutput.printedWordCloud.keys should not contain ("today", "scala", "programming")
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
    mockOutput.printedWordCloud.keys should not contain ("hello")
    mockOutput.printedWordCloud.keys should contain("there")
    mockOutput.printedWordCloud.keys should contain ("world")
    mockOutput.printedWordCloud.keys should not contain ("hi")
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
    mockOutput.printedWordCloud.keys should contain("banana") // Should be counted
    mockOutput.printedWordCloud.keys should not contain ("apple", "orange", "grape")
    
    
  }

  it should "respect the cloud size limit" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("apple", "banana", "apple", "orange")
    val cloudSize = 1
    val minLength = 3
    val windowSize = 10
    val minFrequency = 1

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check the printed word cloud respects the cloud size
    mockOutput.printedWordCloud.size shouldEqual 1
  }

  it should "sort the word cloud by frequency" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("apple", "banana", "apple", "banana", "apple", "orange")
    val cloudSize = 2
    val minLength = 3
    val windowSize = 10
    val minFrequency = 1

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check the printed word cloud is sorted by frequency
    val sortedWords = mockOutput.printedWordCloud.toSeq.sortBy(-_._2)
    sortedWords shouldEqual Seq(("apple", 3), ("banana", 2))
  }

  it should "filter words by minimum frequency" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("apple", "banana", "apple", "orange", "banana", "banana", "grape")
    val cloudSize = 3
    val minLength = 3
    val windowSize = 10
    val minFrequency = 3

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Check that only words with frequency >= minFrequency are included
    mockOutput.printedWordCloud shouldEqual Map("banana" -> 3)
  }

  it should "remove words from the cloud when their frequency drops to zero" in {
    val mockOutput = new MockOutputSink

    val words = Iterator("apple", "apple", "orange", "apple", "banana", "banana", "banana")
    val cloudSize = 3
    val minLength = 3
    val windowSize = 3 // Set small window size to trigger removals
    val minFrequency = 1

    // Run the wordcloud function
    Main.wordcloud(words, cloudSize, minLength, windowSize, minFrequency, mockOutput)

    // Verify the word "orange" is removed from the word cloud after it exits the window
    mockOutput.printedWordCloud should not contain ("orange")
  }

  it should "parse command-line arguments correctly" in {
    val args = Array("-c", "5", "-l", "4", "-w", "50", "-s", "5", "-f", "2")
    Main.main(args)
    // Add assertions to verify the behavior with the given arguments
  }

}
