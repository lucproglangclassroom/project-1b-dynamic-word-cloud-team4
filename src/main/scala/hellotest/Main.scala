package hellotest

import mainargs.{main, arg, ParserForMethods}

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
    wordcloud(words, cloudSize, minLength, windowSize, minFrequency, everyKSteps, output)

  
  // Word cloud generation logic as functional
  def wordcloud(
      words: Iterator[String], 
      cloudSize: Int, 
      minLength: Int, 
      windowSize: Int, 
      minFrequency: Int, 
      everyKSteps: Int,
      output: OutputSink
  ): Unit =
    case class State(window: List[String], freqMap: Map[String, Int])
    // Function to update state (window and frequency map) as we scan through words
    def updateState(state: State, word: String): State =
      val filteredWord = word.length >= minLength
      val updatedWindow = if (filteredWord) word :: state.window else state.window
      val window = if (updatedWindow.length > windowSize) updatedWindow.take(windowSize) else updatedWindow

      // Remove the word that was dequeued (FIFO behavior)
      val dequeuedWord = if (updatedWindow.length > windowSize) Some(updatedWindow(windowSize)) else None

      // Update frequency map
      val updatedFreqMap = state.freqMap
        .updated(word, state.freqMap.getOrElse(word, 0) + 1)
        .updated(dequeuedWord.getOrElse(""), 0) // Remove the dequeued word if it exists

      // Clean up the freqMap to remove words with zero count
      val finalFreqMap = updatedFreqMap.filter(_._2 > 0)

      State(window, finalFreqMap)

    // Scan through words and update state at each step
    val initialState = State(Nil, Map.empty[String, Int])

    // Process each word and update the state
    val states = words.scanLeft(initialState)(updateState)

    // Generate and output the word cloud at each step
    states.zipWithIndex.foreach { case (state, index) =>
      val freqMap = state.freqMap
      // Generate word cloud from the frequency map, filtering by minFrequency and taking the top `cloudSize`
      val sortedWords = freqMap.toSeq
        .filter(_._2 >= minFrequency)
        .sortBy(-_._2)
        .take(cloudSize)
        .toMap

      // Output the word cloud if needed
      if (index % everyKSteps == 0) // Only output every K steps
        output.print(sortedWords)
    }

end Main