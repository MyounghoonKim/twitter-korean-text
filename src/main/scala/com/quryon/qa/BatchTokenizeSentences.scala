package com.quryon.qa

import java.util.logging.{Level, Logger}

import com.twitter.penguin.korean.TwitterKoreanProcessor
import com.twitter.penguin.korean.tokenizer.KoreanTokenizer.KoreanToken
import com.twitter.penguin.korean.util.KoreanPos

// file input and output
import scala.io._
import java.io._

import com.twitter.penguin.korean.qa.BatchTokenizeTweets

object BatchTokenizeSentences {
  import BatchTokenizeTweets._

  private val LOG = Logger.getLogger(getClass.getSimpleName)

  def main(args: Array[String]) {
    println("===========================================")
    println("BatchTokenizeSentences.main()")
    println(args)

    if (args.length != 2) {
      println("The first arg should be an input file of Korean sentences and the second args should be an output file for parsed result.")
      return
    }
    val pw = new PrintWriter(new File(args(1)))
    val parseTimesAll = Source.fromFile(args(0)).getLines().foldLeft(List[ParseTime]()) {
      case (l: List[ParseTime], line: String) =>
        val t0 = System.currentTimeMillis()
        val normalized = TwitterKoreanProcessor.normalize(line)
        val parsed = TwitterKoreanProcessor.tokenize(normalized)
        val t1 = System.currentTimeMillis()

        println()

        val to_print = parsed.map { t =>
          if (t.pos.toString == "Space")
            ""
          else
            t.text + "/" + t.pos
        }.mkString(" ").replace("  ", " ") + "\n"


        println(to_print)
        pw.write(to_print)
        ParseTime(t1 - t0, line.trim) :: l
    }
    pw.close()

    val loadingTime = parseTimesAll.last

    LOG.log(Level.INFO, "\n\n\n\nThe first one \"%s\" took %d ms including the loading time.".format(loadingTime.chunk, loadingTime.time))

    val parseTimes = parseTimesAll.init

    val averageSentenceLength = parseTimes.map(_.chunk.length).sum.toDouble / parseTimes.size

    val averageTime = parseTimes.map(_.time).sum.toDouble / parseTimes.size
    val maxItem = parseTimes.maxBy(_.time)

    LOG.log(Level.INFO, ("Parsed %d items. \n" +
        "       Total time: %d s \n" +
        "       Average sentence length: %.2f chars \n" +
        "       Average time per sentence: %.2f ms \n" +
        "       Max time: %d ms, %s\n" +
        "       Parsed result of Max time sentence: %s" +
        "\n\n\n"
        ).format(
          parseTimes.size,
          parseTimes.map(_.time).sum / 1000,
          averageSentenceLength,
          averageTime,
          maxItem.time,
          maxItem.chunk,
          TwitterKoreanProcessor.tokenize(maxItem.chunk).map {
            case t if t.unknown => t.text.toString + t.pos + "*"
            case t => t.text + t.pos.toString
          }.mkString(" ")
        ))
  }
}