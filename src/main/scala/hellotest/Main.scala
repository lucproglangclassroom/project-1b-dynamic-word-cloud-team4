package edu.luc.cs.consoleapp

import scala.io.Source
import org.apache.commons.collections4.queue.CircularFifoQueue
import scala.util.{Try, Success, Failure}
import mainargs._

// Define a logging trait for diagnostics
trait Logger {
  def log(message: String): Unit
}

object ConsoleLogger extends Logger {
  def log(message: String): Unit = println(message)
}

@main
def topwords(
    @arg(name = "last-n-words", short = 'n') lastNWords: Int = 10
): Unit = {
  val logger: Logger = ConsoleLogger
  val queue = new CircularFifoQueue[String](lastNWords)

  val input = Source.stdin.getLines()
  input.foreach { line =>
    line.split("(?U)[^\\p{Alpha}0-9']+").foreach { word =>
      if (word.nonEmpty) {
        queue.add(word) // Automatically evicts the oldest word
        logger.log(queue.toString) // Log the current state of the queue
        if (System.out.checkError()) {
          System.exit(1) // Handle SIGPIPE
        }
      }
    }
  }
}
