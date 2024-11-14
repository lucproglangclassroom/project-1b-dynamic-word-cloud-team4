package hellotest

import mainargs.{main, arg, ParserForMethods}

type Cloud = (Int, Seq[(String, Long)])

trait OutputObserver:
  def doOutput(cloud: Cloud): Boolean

trait OutputToStdout extends OutputObserver:
  override def doOutput(cloud: Cloud): Boolean =
    print(s"${cloud._1}: words { ")
    print(cloud._2.map(_.productIterator.mkString(": ")).mkString(" "))
    println(" }")
    !sys.process.stdout.checkError()

trait WordCloud extends OutputObserver:
  def computeWordClouds(words: Iterator[String], howMany: Int, lastNWords: Int, everyKSteps: Int, minFrequency: Int): Unit

trait Main extends WordCloud with OutputToStdout:

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toIndexedSeq)

  @main
  def run(
           @arg(short = 'c', doc = "size of the sliding word cloud") cloudSize: Int = 10,
           @arg(short = 'l', doc = "minimum word length to be considere") minLength: Int = 6,
           @arg(short = 'w', doc = "size of the sliding FIFO queue") windowSize: Int = 1000,
           @arg(short = 's', doc = "number of steps between word cloud updates") everyKSteps: Int = 10,
           @arg(short = 'f', doc = "minimum frequency for a word to be included in the cloud") minFrequency: Int = 3) =

    val lines = scala.io.Source.stdin.getLines()
    val words =
      import scala.language.unsafeNulls
      lines
        .flatMap(l => l.split("[^\\p{L}]+"))
        .withFilter(_.length >= minLength)
        .map(_.toLowerCase)

    computeWordClouds(words, cloudSize, windowSize, everyKSteps, minFrequency)

end Main