package edu.luc.cs.consoleapp

import scala.io.Source
import org.apache.commons.collections4.queue.CircularFifoQueue
import scala.language.unsafeNulls
import mainargs.{main, arg, ParserForMethods}
import java.lang.System
import scala.io.StdIn.{readLine, readInt}

// Define a logging trait for diagnostics
trait Logger {
  def log(message: String): Unit
}

trait OutputSink:
  def print(wordCloud: Map[String, Int]): Unit

object ConsoleLogger extends Logger {
  def log(message: String): Unit = println(message)
}

class OutputToConsole extends OutputSink:
  override def print(wordCloud: Map[String, Int]): Unit =
    println("Word cloud: ")
    wordCloud.foreach { case (word, count) =>
      println(s"$word: $count")
    }

@main
object Main {
  def main(args: Array[String]): Unit = {
    topwords(lastNWords = 10) // Call your main function with default arguments
  }
  
  def run(
    @arg(short = 'c', doc = "size of the sliding word cloud") cloudSize: Int = 10,
    @arg(short = 'l', doc = "minimum word length to be considered") minLength: Int = 6,
    @arg(short = 'w', doc = "size of the sliding FIFO queue") windowSize: Int = 1000,
    @arg(short = 's', doc = "number of steps between word cloud updates") everyKSteps: Int = 10,
    @arg(short = 'f', doc = "minimum frequency for a word to be included in the cloud") minFrequency: Int = 3
  ): Unit = {
  
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
      val words = Iterator("programming", "scala", "programming", "language", "tomorrow","tomorrow", "hello", "world", "scala", "programming", "tomorrow", "programming", "functional")

      // Call the wordcloud function with all required parameters
      wordcloud(words, cloudSize, minLength, windowSize, minFrequency, output)
  }

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
      .toList // convert to lsit for processing
      .sliding(windowSize) // create sliding window

    val freqMap = slidingWindows.foldLeft(Map.empty[String, Int]) {( acc,window) =>
      window.foldLeft(acc) { (innerAcc, word) =>
        if (word != null) {
          val updatedCount = innerAcc.getOrElse(word, 0) + 1
          innerAcc.updated(word, updatedCount) // create a new map with the updated frequency
        } else {
          innerAcc 
        }
    }
  }
    // Sort and print the word cloud
    val cloud = freqMap.toSeq.sortBy(-_._2).take(cloudSize).toMap
    if (cloud.nonEmpty) output.print(cloud)
  }
    //val cloud = freqMap.toSeq.sortBy(-_._2).take(cloudSize).toMap 
   // println(s"Generated word cloud: $cloud")

    //if(cloud.nonEmpty) output.print(cloud)
  def topwords(
    @arg(name = "last-n-words", short = 'n') lastNWords: Int = 10): Unit = {
      val logger: Logger = ConsoleLogger
      var queue = List.empty[String] // immutable List
      val input = Source.stdin.getLines()
      input.foreach { line =>
        val words = line.split("(?U)[^\\p{Alpha}0-9']+").filter(_.nonEmpty)
        words.foreach { word =>
          queue = (word :: queue).take(lastNWords) // update the queue
          logger.log(queue.toString) // Log the current state of the queue
          if(System.out.checkError()) {
            System.exit(1) // Handle SIGPIPE
          }
        }
      }
    }
  }
