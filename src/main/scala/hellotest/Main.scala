package hellotest

import mainargs.{main, arg, ParserForMethods}
import scala.collection.mutable

// Define OutputSink for output handling
trait OutputSink:
  def print(wordCloud: Map[String, Int]): Unit

// OutputSink that prints to the console
class OutputToConsole extends OutputSink:
  override def print(wordCloud: Map[String, Int]): Unit =
    println("Word cloud:")
    wordCloud.foreach { case (word, count) =>
      println(s"$word: $count")
    }

// Main object with argument parsing and word cloud logic
object Main:

  // External entry point into the Scala application
  def main(args: Array[String]): Unit = 
    //ParserForMethods(this).runOrExit(args.toIndexedSeq)
    ParserForMethods(Main).runOrExit(args.toIndexedSeq)

  // Internal main method with arguments annotated for parsing
  @main
  def run(
    @arg(short = 'c', doc = "size of the sliding word cloud") cloudSize: Int = 10,
    @arg(short = 'l', doc = "minimum word length to be considered") minLength: Int = 6,
    @arg(short = 'w', doc = "size of the sliding FIFO queue") windowSize: Int = 1000,
    @arg(short = 's', doc = "number of steps between word cloud updates") everyKSteps: Int = 10,
    @arg(short = 'f', doc = "minimum frequency for a word to be included in the cloud") minFrequency: Int = 3
  ): Unit =
  
    println("Welcome to the Word Cloud Generator!")
    println(s"Today's date: ${java.time.LocalDate.now}")
    println("\nYou provided the following command-line arguments:")
    println(s"- Cloud Size: $cloudSize")
    println(s"- Minimum Word Length: $minLength")
    println(s"- Window Size: $windowSize")
    println(s"- Steps Between Updates: $everyKSteps")
    println(s"- Minimum Frequency: $minFrequency")

    // Create an instance of OutputToConsole
    val output = new OutputToConsole

    // Example words
    val words = Iterator("hello", "world", "hello", "today", "world", "hello", "scala", "programming")

    // Call the wordcloud function with all required parameters
    wordcloud(words, cloudSize, minLength, windowSize, minFrequency, output)

  
  // Word cloud generation logic
  def wordcloud(
    words: Iterator[String], 
    cloudSize: Int, 
    minLength: Int, 
    windowSize: Int, 
    minFrequency: Int, 
    output: OutputSink
  ): Unit = {
    val window = new mutable.Queue[String]()
    val wordFrequencies = mutable.Map[String, Int]()
    
    words.foreach { word =>
      if (word.length >= minLength) {  // Check minimum length
        // Add word to the sliding window
        window.enqueue(word)
        
        // Update the frequency for the current word
        wordFrequencies(word) = wordFrequencies.getOrElse(word, 0) + 1

        // Maintain the size of the sliding window
        if (window.size > windowSize) {
          // Remove the oldest word from the window
          val removedWord = window.dequeue()
          
          // Decrement its frequency in the map
          if (wordFrequencies.contains(removedWord)) {
            wordFrequencies(removedWord) -= 1

            // If frequency drops to zero, remove it from the map
            if (wordFrequencies(removedWord) <= 0) {
              wordFrequencies.remove(removedWord)
            }
          }
        }
      }
    }
    
    // Generate and output the word cloud
    val sortedWords = wordFrequencies.toSeq
      .filter(_._2 >= minFrequency) // Only include words meeting the minimum frequency
      .sortBy(-_._2) // Sort by frequency descending
      .take(cloudSize) // Take the top N words based on cloud size
      .toMap // Convert back to a Map

    output.print(sortedWords)
  }
end Main