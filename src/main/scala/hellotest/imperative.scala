package hellotest

import scala.collection.mutable.{ ArrayBuffer, Map, Queue }

object MainCloudSimple extends Main with CloudSimple

trait CloudSimple extends WordCloud:

  def computeWordClouds(words: Iterator[String], howMany: Int, lastNWords: Int, everyKSteps: Int, minFrequency: Int): Unit =

    val cloudQueue = Queue.empty[String]
    var cycle = 0

    for word <- words do
      cycle += 1
      // update queue
      cloudQueue.enqueue(word)

      // start producing updates every k steps once queue reaches capacity
      if cloudQueue.length >= lastNWords then

        if cloudQueue.length > lastNWords then
          cloudQueue.dequeue()

        if cycle % everyKSteps == 0 then
          // build frequency table from queue
          val wordFrequencies = Map.empty[String, Long]
          for word <- cloudQueue do
            val sum = wordFrequencies.getOrElse(word, 0L)
            wordFrequencies.update(word, sum + 1)

          // prepare text-based word cloud sorted in descending order of frequency
          val wordCloud = ArrayBuffer.empty[(String, Long)]
          for
            (w, f) <- wordFrequencies
            if f >= minFrequency
          do
            wordCloud += ((w, f))

          wordCloud.sortInPlaceBy(_._2)(math.Ordering.Long.reverse)
          // print only the most frequent words and check for SIGPIPE
          if !doOutput(cycle, wordCloud.take(howMany).toSeq) then sys.exit(1)
    end for

end CloudSimple