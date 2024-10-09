package hellotest

import mainargs.{main, arg, ParserForMethods}

// Define OutputSink for output handling
trait OutputSink:
  def print(wordCloud: Map[String, Int]): Unit

// OutputSink that prints to the console
class OutputToConsole extends OutputSink:
  override def print(wordCloud: Map[String, Int]): Unit =
    println("Word cloud: ")
    wordCloud.foreach { case (word, count) =>
      println(s"$word: $count")
    }

// Main object with argument parsing and word cloud logic
object Main:

  // External entry point into the Scala application
  def main(args: Array[String]): Unit = 
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

  
  // Word cloud generation logic as functional
  def wordcloud(
      words: Iterator[String], 
      cloudSize: Int, 
      minLength: Int, 
      windowSize: Int, 
      minFrequency: Int,
      output: OutputSink
    ): Unit = {
    
    // sliding window implemented using scanLeft
    val slidingWindows = words
      .filter(_.length >= minLength) // filter by word length
      .scanLeft(List.empty[String]) {(window, word) =>
        (word :: window).take(windowSize) // keep the window size fixed
      }
    
    // calculate word frequencies and generate word cloud for each sliding window
    val wordClouds = slidingWindows.map{window =>
      window.groupBy(identity) // group by word
        .view.mapValues(_.size) // count occurrences
        .filter(_._2 >= minFrequency) // filter by minimum frequency
        .toMap
        .toSeq.sortBy(-_._2) // sort by frequency
        .take(cloudSize) // take top n words
        .toMap // convert back to map for output
    }
    // output the final word cloud
    wordClouds.foreach(output.print)
  
  }

end Main