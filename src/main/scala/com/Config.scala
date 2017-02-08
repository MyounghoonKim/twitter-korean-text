package com

trait Config {
  var nounDictFiles = Seq[String]()
  var verbDictFiles = Seq[String]()
  def isGood() = {
    if (nounDictFiles.isEmpty) {
      false
    } else {
      true
    }
  }
}

class EmptyConfig extends Config {}

class DefaultConfig extends Config {
  nounDictFiles = Seq(
    "noun/nouns.txt", "noun/entities.txt", "noun/spam.txt",
    "noun/names.txt", "noun/twitter.txt", "noun/lol.txt",
    "noun/slangs.txt", "noun/company_names.txt",
    "noun/foreign.txt", "noun/geolocations.txt", "noun/profane.txt",
    "substantives/given_names.txt", "noun/kpop.txt", "noun/bible.txt",
    "noun/pokemon.txt", "noun/congress.txt", "noun/wikipedia_title_nouns.txt"
  )
}

class CosmeticsAddedConfig extends Config {
  nounDictFiles ++= Seq("noun/cosmetics.txt")
}

class LolOnlyConfig extends Config {
  nounDictFiles = Seq("noun/lol.txt")
}

