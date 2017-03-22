package com.quryon.qa

import java.util.logging.{Level, Logger}

import com.twitter.penguin.korean.qa.BatchTokenizeTweets

object BatchTokenizeSentences {
  import BatchTokenizeTweets.{main => bttMain}
  def main(args: Array[String]) {
    println("===========================================")
    println("BatchTokenizeSentences.main()")
    println(args)
    bttMain(args)
  }
}