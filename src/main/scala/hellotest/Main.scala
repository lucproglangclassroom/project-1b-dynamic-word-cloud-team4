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
    ParserForMethods(this).runOrExit(args.toIndexedSeq)
    ()

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

    // Example words, this could come from any source (file, stdin, etc.)
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
  ): Unit =
    val window = new mutable.Queue[String]()
    val wordFrequencies = mutable.Map[String, Int]()
    
    words.foreach { word =>
      if (word.length >= minLength) {  // Check minimum length
        window.enqueue(word)
        if (window.size > windowSize) window.dequeue()

        // Update frequencies
        wordFrequencies.clear()
        window.foreach { w => 
          wordFrequencies(w) = wordFrequencies.getOrElse(w, 0) + 1
        }
        
        // Generate and output word cloud
        val sortedWords = wordFrequencies.toSeq
          .filter(_._2 >= minFrequency)
          .sortBy(-_._2)
          .take(cloudSize)
          .toMap
        
        output.print(sortedWords)
      }
    }

end Main
