/*
 * Twitter Korean Text - Scala library to process Korean text
 *
 * Copyright 2014 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.twitter.penguin.korean.tokenizer

import com.twitter.penguin.korean.tokenizer.KoreanChunker._
import com.twitter.penguin.korean.util.KoreanDictionaryProvider._
import com.twitter.penguin.korean.util.KoreanPos
import com.twitter.penguin.korean.util.KoreanPos._
import com.twitter.penguin.korean.util.KoreanSubstantive._

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Provides Korean tokenization.
  *
  * Chunk: 어절 - 공백으로 구분되어 있는 단위 (사랑하는사람을)
  * Word: 단어 - 하나의 문장 구성 요소 (사랑하는, 사람을)
  * Token: 토큰 - 형태소와 비슷한 단위이지만 문법적으로 정확하지는 않음 (사랑, 하는, 사람, 을)
  *
  * Whenever there is an updates in the behavior of KoreanParser,
  * the initial cache has to be updated by running tools.CreateInitialCache.
  */
object KoreanTokenizer {
  private val TOP_N_PER_STATE = 5
  private val MAX_TRACE_BACK = 8
  /**
    * 0 for optional, 1 for required
    * * for optional repeatable, + for required repeatable
    *
    * Substantive: 체언 (초거대기업의)
    * Predicate: 용언 (하였었습니다, 개예뻤었다)
    * Modifier: 수식언 (모르는 할수도있는 보이기도하는 예뻐 예쁜 완전 레알 초인간적인 잘 잘한)
    * Standalone: 독립언
    * Functional: 관계언 (조사)
    *
    * N Noun: 명사 (Nouns, Pronouns, Company Names, Proper Noun, Person Names, Numerals, Standalone, Dependent)
    * V Verb: 동사 (하, 먹, 자, 차)
    * J Adjective: 형용사 (예쁘다, 크다, 작다)
    * A Adverb: 부사 (잘, 매우, 빨리, 반드시, 과연)
    * D Determiner: 관형사 (새, 헌, 참, 첫, 이, 그, 저)
    * E Exclamation: 감탄사 (헐, ㅋㅋㅋ, 어머나, 얼씨구)
    *
    * C Conjunction: 접속사
    *
    * j SubstantiveJosa: 조사 (의, 에, 에서)
    * l AdverbialJosa: 부사격 조사 (~인, ~의, ~일)
    * e Eomi: 어말어미 (다, 요, 여, 하댘ㅋㅋ)
    * r PreEomi: 선어말어미 (었)
    *
    * p NounPrefix: 접두사 ('초'대박)
    * v VerbPrefix: 동사 접두어 ('쳐'먹어)
    * s Suffix: 접미사 (~적)
    */
  private val operatorDesc = "0: / *: 없어도 됨 / 1: must / "
  private val SequenceDefinition = Map(
    // study
    // +나 1이 없으면 final
    // "D0p0" -> Noun // 1 candi, 2 trie
                                // (Determiner, NounPrefix)
                                // (NounPrefix)
    // "D0p0" -> Noun,
    // "A1" -> Adverb   // 1 candi, 3 trie
                                // (Determiner, NounPrefix)
                                // (NounPrefix)
                                // (Adverb)

    // "D*p0" -> Noun // 1 candi, 2 trie
                                // (Determiner, NounPrefix)
                                // (NounPrefix)
    // "D1p0" -> Noun // 1 candi, 1 trie
                                // (Determiner, NounPrefix)
    // "D+p0" -> Noun // 1 candi, 2 trie
                                // (Determiner, NounPrefix)

    // "D0p*N1" -> Noun, // 1 candi,
    // "A1" -> Adverb    // 3 trie
                          // (Determiner, NounPrefix, Noun), (Determiner, Noun)
                          // (NounPrefix, Noun)
                          // (Noun)
                          // (Adverb)

    // "D*p*N1" -> Noun // 1 candi,
                        // 3 trie
                          // (Determiner, NounPrefix, Noun), (Determiner, Noun)
                          // (NounPrefix, Noun)
                          // (Noun)

    // "D+p*N1" -> Noun // 1 candi,
                        // 1 trie
                          // (Determiner, NounPrefix, Noun), (Determiner, Noun)

    // "D0p*N1s0" -> Noun // 1 candi,
                          // 3 trie
                            // (Det, NPrefix, N, Suf), (Det, N, Suf)
                            // (NPrefix, N, Suf)
                            // (N, Suf)

    // "D0p*N1s0j0" -> Noun // 1 candi,
                          // 3 trie
                            // (Det, NPrefix, N, Suf, J), (Det, NPrefix, N, J), (Det, N, Suf, J), (Det, N, J)
                            // (NPrefix, N, Suf, J), (NPrefix, N, J)
                            // (N, Suf, J), (N, J)

    // "v*V1r*e0" -> Verb
    // Substantive
    "D0p*N1s0j0" -> Noun,
    // Predicate 초기뻐하다, 와주세요, 초기뻤었고, 추첨하다, 구경하기힘들다, 기뻐하는, 기쁜, 추첨해서, 좋아하다, 걸려있을
    "v*V1r*e0" -> Verb,
    "v*J1r*e0" -> Adjective,
    // Modifier 부사
    "A1" -> Adverb,
    // Standalone
    "C1" -> Conjunction,
    "E+" -> Exclamation,
    "j1" -> Josa
  )
  private val koreanPosTrie = KoreanPos.getTrie(SequenceDefinition)
  /**
    * Parse Korean text into a sequence of KoreanTokens with custom parameters
    *
    * @param text Input Korean chunk
    * @return sequence of KoreanTokens
    */
  def tokenize(
        text: CharSequence,
        profile: TokenizerProfile = TokenizerProfile.defaultProfile,
        verboseLevel: Int = 0
      ): Seq[KoreanToken] = {
    try {
      chunk(text).flatMap {
        case token: KoreanToken if token.pos == Korean =>
          // Get the best parse of each chunk
          val parsed = if (verboseLevel == 0) {
            parseKoreanChunk(token, profile)
          } else if (verboseLevel == 1) { // 약간 verboseLevel
            parseKoreanChunkVerbose(token, profile)
          } else if (verboseLevel == 2) { // 완전 verbose
            parseKoreanChunkVeryVerbose(token, profile)
          } else {
            parseKoreanChunk(token, profile)
          }

          // Collapse sequence of one-char nouns into one unknown noun: (가Noun 회Noun -> 가회Noun*)
          collapseNouns(parsed)
        case token: KoreanToken => Seq(token)
      }
    } catch {
      case e: Exception =>
        System.err.println(s"Error tokenizing a chunk: $text")
        throw e
    }
  }


  /**
    * Find the best parse using dynamic programming.
    *
    * @param chunk Input chunk. The input has to be entirely. Check for input validity is skipped
    *              for performance optimization. This method is private and is called only by tokenize.
    * @return The best possible parse.
    */
  private[this] def parseKoreanChunk(chunk: KoreanToken, profile: TokenizerProfile = TokenizerProfile.defaultProfile): Seq[KoreanToken] = {
    // Direct match
    // This may produce 하 -> PreEomi
    koreanDictionary.foreach {
      case (pos, dict) =>
        if (dict.contains(chunk.text)) {
          return Seq(KoreanToken(chunk.text, pos, chunk.offset, chunk.length))
        }
    }

    // Buffer for solutions
    val solutions: mutable.Map[Int, List[CandidateParse]] = new java.util.HashMap[Int, List[CandidateParse]]

    // Initial state
    solutions += 0 -> List(
      CandidateParse(
        ParsedChunk(Seq[KoreanToken](), 1, profile),
        koreanPosTrie, ending = None
      )
    )

    // Find N best parses per state
    for (
      end <- 1 to chunk.length;
      start <- end - 1 to(Seq(end - MAX_TRACE_BACK, 0).max, -1)
    ) {
      val word = chunk.text.slice(start, end)

      val curSolutions = solutions(start)

      val candidates = curSolutions.flatMap {
        solution =>
          val possiblePoses: Seq[PossibleTrie] = if (solution.ending.isDefined) {
            solution.curTrie.map(t => PossibleTrie(t, 0)) ++ koreanPosTrie.map(
              t => PossibleTrie(t, 1))
          } else {
            solution.curTrie.map(t => PossibleTrie(t, 0))
          }

          possiblePoses.view.filter { t =>
            t.curTrie.curPos == Noun || koreanDictionary(t.curTrie.curPos).contains(
              word.toCharArray)
          }.map { case t: PossibleTrie =>
            val candidateToAdd =
              if (t.curTrie.curPos == Noun && !koreanDictionary(Noun).contains(word.toCharArray)) {
                val isWordName: Boolean = isName(word)
                val isWordKoreanNameVariation: Boolean = isKoreanNameVariation(word)

                val unknown = !isWordName && !isKoreanNumber(word) && !isWordKoreanNameVariation
                val pos = if (unknown || isWordName || isWordKoreanNameVariation) ProperNoun else Noun
                ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length, unknown)),
                  t.words, profile)
              } else {
                val pos = if (t.curTrie.curPos == Noun && properNouns.contains(
                  word.toCharArray)) ProperNoun
                else t.curTrie.curPos
                ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length)), t.words,
                  profile)
              }

            val nextTrie = t.curTrie.nextTrie.map {
              case nt: KoreanPosTrie if nt == selfNode => t.curTrie
              case nt: KoreanPosTrie => nt
            }

            CandidateParse(solution.parse ++ candidateToAdd, nextTrie, t.curTrie.ending)
          }
      }

      val currentSolutions = if (solutions.contains(end)) solutions(end) else List()

      solutions += end -> (currentSolutions ++ candidates).sortBy {
        c => (c.parse.score, c.parse.posTieBreaker)
      }.take(TOP_N_PER_STATE)
    }


    if (solutions(chunk.length).isEmpty) {
      // If the chunk is not parseable, treat it as a unknown noun chunk.
      Seq(KoreanToken(chunk.text, Noun, 0, chunk.length, true))
    } else {
      // Return the best parse of the final state
      solutions(chunk.length).minBy(c => c.parse.score).parse.posNodes
    }
  }

  private[this] def parseKoreanChunkVerbose(chunk: KoreanToken, profile: TokenizerProfile = TokenizerProfile.defaultProfile): Seq[KoreanToken] = {
    // Direct match
    // This may produce 하 -> PreEomi
    println(s"-parseKoreanChunk (${chunk.text})-")
    koreanDictionary.foreach {
      case (pos, dict) =>
        if (dict.contains(chunk.text)) {
          println("--It's in dictionary")
          println("--Return this:")
          println(Seq(KoreanToken(chunk.text, pos, chunk.offset, chunk.length)))
          return Seq(KoreanToken(chunk.text, pos, chunk.offset, chunk.length))
        }
    }

    // Buffer for solutionTrees
    val solutionTrees: mutable.Map[Int, List[CandidateParse]] = new java.util.HashMap[Int, List[CandidateParse]]

    // Initial state
    solutionTrees += 0 -> List(
      CandidateParse(
        ParsedChunk(Seq[KoreanToken](), 1, profile),
        koreanPosTrie, ending = None
      )
    )
    val baseParsedChunk = ParsedChunk(Seq[KoreanToken](), 1, profile)
    println("-Find N best parses per state")
    for (
      end <- 1 to chunk.length;
      start <- end - 1 to(Seq(end - MAX_TRACE_BACK, 0).max, -1)
    ) {
      val word = chunk.text.slice(start, end)
      println("*********************")
      println(s"word: $word")
      println("*********************")
      val solutionTreesAtStart = solutionTrees(start)
      val candidates = solutionTreesAtStart.flatMap { solution =>
        println(s"solution.ending: ${solution.ending}")
        println("solution.ending이 정의되어 있으면 ")
        if (solution.ending.isDefined) {
          println(s"koreanPosTrie.size: ${koreanPosTrie.size}")
          println("SequenceDefinition으로 만든 Trie를 모두 추가")
        } else {
          println("아니면 그냥 현재 solution만 추가")
        }
        val possibleTries: Seq[PossibleTrie] = if (solution.ending.isDefined) {
          solution.curTrie.map(t => PossibleTrie(t, 0)) ++ koreanPosTrie.map(
            t => PossibleTrie(t, 1))
        } else {
          solution.curTrie.map(t => PossibleTrie(t, 0))
        }
        println(s"----possibleTries.size: ${possibleTries.size}")
        println("------ 더할 캔디데이트를 만든다")
        val candidateParsesToReturn = possibleTries.view.filter { t =>
          if (t.curTrie.curPos == Noun || koreanDictionary(t.curTrie.curPos).contains(
                      word.toCharArray)) {
            println(s"------필터조건: 현재 POS(${t.curTrie.curPos})가 Noun이거나 현재워드($word) is in ${t.curTrie.curPos} 사전")
          }
          t.curTrie.curPos == Noun || koreanDictionary(t.curTrie.curPos).contains(
            word.toCharArray)
        }.map { case t: PossibleTrie =>
          println("-------- 아래는 candidate이 되기 위한 조건들")
          val candidateToAdd =
            if (t.curTrie.curPos == Noun && !koreanDictionary(Noun).contains(word.toCharArray)) {
              println(s"---------- Noun인데 사전에 ${word}가 없다")
              val isWordName: Boolean = isName(word)
              val isWordKoreanNameVariation: Boolean = isKoreanNameVariation(word)

              val unknown = !isWordName && !isKoreanNumber(word) && !isWordKoreanNameVariation
              val pos = if (unknown || isWordName || isWordKoreanNameVariation) ProperNoun else Noun
              val pc = ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length, unknown)),
                t.words, profile)
              ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length, unknown)),
                t.words, profile)
            } else {
              println(s"---------- Noun이거나 Noun 사전에 ${word}이/가 있다")
              val pos = if (t.curTrie.curPos == Noun && properNouns.contains(
                word.toCharArray)) {
                println(s"-------------- Noun이고 poperNoun 사전에 ${word}이/가 있으면 pos = ProperNoun")
                ProperNoun
              } else {
                println(s"-------------- Noun이 아니거나 poperNoun 사전에 ${word}이/가 없거나 하면 pos = t.curTrie.curPos(${t.curTrie.curPos})")
                t.curTrie.curPos
              }
              val pc = ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length)), t.words,
                profile)
              ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length)), t.words,
                profile)
            }
          val nextTrie = t.curTrie.nextTrie.map { case nt: KoreanPosTrie =>
            if (nt == selfNode) {
              t.curTrie
            } else {
              nt
            }
          }
          val nextTrieOriginal = t.curTrie.nextTrie.map {
            case nt: KoreanPosTrie if nt == selfNode => t.curTrie
            case nt: KoreanPosTrie => nt
          }
          assert(nextTrieOriginal == nextTrie)
          val returnCandidate = CandidateParse(solution.parse ++ candidateToAdd, nextTrie, t.curTrie.ending)
          returnCandidate
        }
        candidateParsesToReturn
      }
      println("--end of solutionTreesAtStart로 candidates 생성")
      println("=====================================================")
      candidates.sortBy {
        c => (c.parse.score, c.parse.posTieBreaker)
      }.flatMap { solution =>
        println(s"${solution.parse.posNodes}, score: ${solution.parse.score}, tiebreaker: ${solution.parse.posTieBreaker}\n ${solution.parse.scoreSpec.mkString("/")}")
        Array(solution)
      }
      println("=====================================================")
      println()
      println()

      val solutionTreesAtEnd = if (solutionTrees.contains(end)) solutionTrees(end) else List()
      solutionTrees += end -> (solutionTreesAtEnd ++ candidates).sortBy {
        c => (c.parse.score, c.parse.posTieBreaker)
      }.take(TOP_N_PER_STATE)

    }

    println("--파이날 솔루션즈")
    println(s"--solutionTrees size: ${solutionTrees.size}")
    println(s"chunk.length: ${chunk.length}")
    solutionTrees.map { s =>
      println("-------------------------------")
      s._2.map{ c =>
        if (!c.parse.posNodes.isEmpty)
          println(s"score: ${c.parse.score}\n${c.parse.scoreSpec.mkString("/")}, posNodes: ${c.parse.posNodes}")
      }
    }

    // println(s"-solutionTrees(chunk.length): ${solutionTrees(chunk.length)}")
    if (solutionTrees(chunk.length).isEmpty) {
      // If the chunk is not parseable, treat it as a unknown noun chunk.
      Seq(KoreanToken(chunk.text, Noun, 0, chunk.length, true))
    } else {
      // Return the best parse of the final state
      // println(s"--chunk.length: ${chunk.length}")
      // println(s"--solutionTrees(chunk.length): ${solutionTrees(chunk.length)}")
      solutionTrees(chunk.length).minBy(c => c.parse.score).parse.posNodes
    }
  }

  private[this] def parseKoreanChunkVeryVerbose(chunk: KoreanToken, profile: TokenizerProfile = TokenizerProfile.defaultProfile): Seq[KoreanToken] = {
    // Direct match
    // This may produce 하 -> PreEomi
    println(s"-parseKoreanChunk (${chunk.text})-")
    koreanDictionary.foreach { case (pos, dict) =>
        if (dict.contains(chunk.text)) {
          println("--It's in dictionary")
          println("--Return this:")
          println(Seq(KoreanToken(chunk.text, pos, chunk.offset, chunk.length)))
          return Seq(KoreanToken(chunk.text, pos, chunk.offset, chunk.length))
        }
    }

    // Buffer for solutionTrees
    val solutionTrees: mutable.Map[Int, List[CandidateParse]] = new java.util.HashMap[Int, List[CandidateParse]]

    // Initial state
    solutionTrees += 0 -> List(
      CandidateParse(
        ParsedChunk(Seq[KoreanToken](), 1, profile),
        koreanPosTrie, ending = None
      )
    )
    val baseParsedChunk = ParsedChunk(Seq[KoreanToken](), 1, profile)
    // println("-baseParsedChunk", baseParsedChunk)
    // println(s"-initial solutionTrees (len: ${solutionTrees.size})")

    // solutionTrees.zipWithIndex.map { s =>
    //   s._1._2.map { ss =>
    //     ss.curTrie.map { t =>
    //       printTrie(t)
    //     }
    //   }
    // }

    // println("-solutionTrees.map { solution =>")
    // solutionTrees.map { solution =>
    //   println("--solution.class", solution.getClass)
    //   println("--solution._1.class", solution._1.getClass)
    //   println("--solution._2.class", solution._2.getClass)
    //   println("--solution._1", solution._1)
    //   println("--candidate_parsers.size", solution._2.size)
    //   println("----solution._2.map { candidate_parsers =>")
    //   solution._2.map { candidate_parsers =>
    //     println("----candidate_parsers.class", candidate_parsers.getClass)
    //     println("----candidate_parsers.parse.class", candidate_parsers.parse.getClass)
    //     println(s"----candidate parser's curTrie count: ${candidate_parsers.curTrie.size}")
    //     println("------candidate_parsers.curTrie.map { z =>")
    //     candidate_parsers.curTrie.map { z =>
    //       println("------each curTrie", z )
    //     }
    //   }
    // }
    // Find N best parses per state
    println("-Find N best parses per state")
    for (
      end <- 1 to chunk.length;
      start <- end - 1 to(Seq(end - MAX_TRACE_BACK, 0).max, -1)
    ) {
      val word = chunk.text.slice(start, end)
      val solutionTreesAtStart = solutionTrees(start)
      println("=======================================================")
      println(s"--solutionTrees size: ${solutionTrees.size}, solutionTreesAtStart.size: ${solutionTreesAtStart.size}, end: $end, start: $start, word: $word")
      println("=======================================================")
      solutionTrees.zipWithIndex.map { case(solutionTree, i) =>
        println(s"------$i -> solutionTree:")
        solutionTree._2.map { solution =>
          println(solution.parse.posNodes)
          solution.curTrie.map { t =>
            printTrie(t)
          }
        }
      }
      println(s"--solutionTreesAtStart로(start:$start) candidates 생성")
      val candidates = solutionTreesAtStart.flatMap { solution =>
        solution.curTrie.zipWithIndex.map { case (t, i) =>
          println(s"------$i solution: (${solution.parse.posNodes})")
          t.nextTrie.map { nt =>
            printTrie(nt)
          }
        }
        println(s"solution.ending: ${solution.ending}")
        println("solution.ending이 정의되어 있으면 ")
        if (solution.ending.isDefined) {
          println(s"koreanPosTrie.size: ${koreanPosTrie.size}")
          println("SequenceDefinition으로 만든 Trie를 모두 추가")
        } else {
          println("아니면 그냥 현재 solution만 추가")
        }
        val possibleTries: Seq[PossibleTrie] = if (solution.ending.isDefined) {
          solution.curTrie.map(t => PossibleTrie(t, 0)) ++ koreanPosTrie.map(
            t => PossibleTrie(t, 1))
        } else {
          solution.curTrie.map(t => PossibleTrie(t, 0))
        }
        println(s"----possibleTries.size: ${possibleTries.size}")
        possibleTries.map { possiblePos =>
          printTrie(possiblePos.curTrie)
        }
        // println("----possibleTries.zipWithIndex.map { case(t, i) =>")
        // possibleTries.zipWithIndex.map { case(t, i) =>
        //   println(s"------each possible Pos ${i+1}/${possibleTries.size}")
        //   println(s"------$SequenceDefinition")
        //   println(s"------t.curTrie.curPos: ${t.curTrie.curPos}")
        //   println(s"------t.curTrie.nextTrie.size: ${t.curTrie.nextTrie.size}")
        //   println(s"------t.curTrie.nextTrie:")
        //   println("------t.curTrie.nextTrie.map { nt =>")
        //   t.curTrie.nextTrie.map { nt =>
        //     printTrie(nt)
        //   }

        // println("----possibleTries.view.filter { t => }.map { case t: PossibleTrie =>")
        println("------ 더할 캔디데이트를 만든다")
        val candidateParsesToReturn = possibleTries.view.filter { t =>
          println(s"------필터조건: 현재 POS(${t.curTrie.curPos})가 Noun이거나 현재워드($word) is in ${t.curTrie.curPos} 사전")
          println(s"filtering...${t.curTrie.curPos == Noun || koreanDictionary(t.curTrie.curPos).contains(
            word.toCharArray)}")
          t.curTrie.curPos == Noun || koreanDictionary(t.curTrie.curPos).contains(
            word.toCharArray)
        }.map { case t: PossibleTrie =>
          val candidateToAdd =
            if (t.curTrie.curPos == Noun && !koreanDictionary(Noun).contains(word.toCharArray)) {
              println(s"---------- Noun인데 사전에 ${word}가 없다")
              val isWordName: Boolean = isName(word)
              val isWordKoreanNameVariation: Boolean = isKoreanNameVariation(word)

              val unknown = !isWordName && !isKoreanNumber(word) && !isWordKoreanNameVariation
              val pos = if (unknown || isWordName || isWordKoreanNameVariation) ProperNoun else Noun
              val pc = ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length, unknown)),
                t.words, profile)
              println(s"--POS 후보: word: $word, pos: $pos, chunk.offset: ${chunk.offset}, start: ${start}, word.length: ${word.length}, unknown: $unknown, t.words: ${t.words}}")
              ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length, unknown)),
                t.words, profile)
            } else {
              println(s"---------- Noun이거나 Noun 사전에 ${word}이/가 있다")
              val pos = if (t.curTrie.curPos == Noun && properNouns.contains(
                word.toCharArray)) {
                println(s"-------------- Noun이고 poperNoun 사전에 ${word}이/가 있으면 pos = ProperNoun")
                ProperNoun
              } else {
                println(s"-------------- Noun이 아니거나 poperNoun 사전에 ${word}이/가 없거나 하면 pos = t.curTrie.curPos(${t.curTrie.curPos})")
                t.curTrie.curPos
              }
              val pc = ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length)), t.words,
                profile)
              println(s"--POS 후보: word: $word, pos: $pos, chunk.offset: ${chunk.offset}, start: ${start}, word.length: ${word.length}, t.words: ${t.words}, tiebreaker: ${pc.posTieBreaker}")
              ParsedChunk(Seq(KoreanToken(word, pos, chunk.offset + start, word.length)), t.words,
                profile)
            }
          // println(s"------candidateToAdd: $candidateToAdd")
          // println(s"------t.curTrie.nextTrie.size: ${t.curTrie.nextTrie.size}")
          // if (t.curTrie.nextTrie.size != 0) println("------val nextTrie = t.curTrie.nextTrie.map { case nt: KoreanPosTrie =>")
          val nextTrie = t.curTrie.nextTrie.map { case nt: KoreanPosTrie =>
            if (nt == selfNode) {
              println(s"--------if (nt == selfNode) { : ${t.curTrie}")
              t.curTrie
            } else {
              println(s"--------else of <if (nt == selfNode)> { : ${nt}")
              nt
            }
          }
          val nextTrieOriginal = t.curTrie.nextTrie.map {
            case nt: KoreanPosTrie if nt == selfNode => t.curTrie
            case nt: KoreanPosTrie => nt
          }
          assert(nextTrieOriginal == nextTrie)
          println(s"------nextTrie: $nextTrie")
          println(s"------t.curTrie.ending: ${t.curTrie.ending}")
          val returnCandidate = CandidateParse(solution.parse ++ candidateToAdd, nextTrie, t.curTrie.ending)
          for (x <- returnCandidate.curTrie) printTrie(x)
          returnCandidate
        }
        println("------ end of 더할 캔디데이트를 만든다")
        candidateParsesToReturn
      }
      println("--end of solutionTreesAtStart로 candidates 생성")
      // println(s"--candidates: $candidates")
      println(s"--candidates.size: ${candidates.size}")
      println("최종적으로 추가할 candidates")
      candidates.sortBy {
        c => (c.parse.score, c.parse.posTieBreaker)
      }.flatMap { solution =>
        println(s"${solution.parse.posNodes}, score: ${solution.parse.score}, tiebreaker: ${solution.parse.posTieBreaker}\n ${solution.parse.scoreSpec.mkString("/")}")
        solution.curTrie.zipWithIndex.map { case (t, i) =>
          println(s"------$i -> $t")
        }
      }

      val solutionTreesAtEnd = if (solutionTrees.contains(end)) solutionTrees(end) else List()
      println(s"--solutionTreesAtEnd(end: $end)")
      println(s"--solutionTreesAtEnd.size: ${solutionTreesAtEnd.size}")
      // println(s"--solutionTreesAtEnd: $solutionTreesAtEnd")
      solutionTrees += end -> (solutionTreesAtEnd ++ candidates).sortBy {
        c => (c.parse.score, c.parse.posTieBreaker)
      }.take(TOP_N_PER_STATE)

      println("기존 솔루션에다가 solutionTreesAtEnd ++ candidates를 더함")
      println("--solutionTrees candidates added")
      solutionTrees.zipWithIndex.map { case (solutionTree, i) =>
        println(s"----$i) solutionTree._1: ${solutionTree._1}")
        solutionTree._2.zipWithIndex.map { case (solution, j) =>
          println(s"------$j)")
          println(s"solution.parse.posNodes: ${solution.parse.posNodes}")
          if (!solution.parse.posNodes.isEmpty) {
            println(s"score: ${solution.parse.score}")
          }
        }
      }
      // println("--solutionTreesAtEnd ++ candidates")
      // (solutionTreesAtEnd ++ candidates).sortBy {
      //   c => (c.parse.score, c.parse.posTieBreaker)
      // }.take(TOP_N_PER_STATE).flatMap { solution =>
      //   solution.curTrie.zipWithIndex.map { case (t, i) =>
      //     println(s"------$i -> $t")
      //   }
      // }

    }

    println("--파이날 솔루션즈")
    println(s"--solutionTrees size: ${solutionTrees.size}")
    println(s"chunk.length: ${chunk.length}")
    solutionTrees.map { s =>
      s._2.map{ c =>
        if (!c.parse.posNodes.isEmpty)
          println(s"score: ${c.parse.score}\n${c.parse.scoreSpec.mkString("/")}, posNodes: ${c.parse.posNodes}")
        // println(s"c.parse.score: ${c.parse.score}")
        // println(s"c.parse.posNodes: ${c.parse.posNodes}")
      }
    }
    // solutionTrees.zipWithIndex.map { case (solution, i) =>
    //   println(s"----${i+1}th solution ")
    //   println(s"----solution._2.size: ${solution._2.size}")
    //   println(s"----solution._2.map { s =>")
    //   solution._2.map { s =>
    //     println(s"------s.parse: ${s.parse}")
    //     println(s"------s.parse.posNodes: ${s.parse.posNodes}")
    //     println(s"------s.parse.words: ${s.parse.words}")
    //     println(s"------s.curTrie.size: ${s.curTrie.size}")
    //     s.curTrie.map { t =>
    //       printTrie(t)
    //     }
    //   }
    // }

    // println(s"-solutionTrees(chunk.length): ${solutionTrees(chunk.length)}")
    if (solutionTrees(chunk.length).isEmpty) {
      // If the chunk is not parseable, treat it as a unknown noun chunk.
      Seq(KoreanToken(chunk.text, Noun, 0, chunk.length, true))
    } else {
      // Return the best parse of the final state
      // println(s"--chunk.length: ${chunk.length}")
      // println(s"--solutionTrees(chunk.length): ${solutionTrees(chunk.length)}")
      solutionTrees(chunk.length).minBy(c => c.parse.score).parse.posNodes
    }
  }

  case class KoreanToken(text: String, pos: KoreanPos, offset: Int, length: Int,
      unknown: Boolean = false) {
    override def toString: String = {
      val unknownStar = if (unknown) "*" else ""
      s"$text$unknownStar(${pos.toString}: $offset, $length)"
    }

    def copyWithNewPos(pos: KoreanPos): KoreanToken = {
      KoreanToken(this.text, pos, this.offset, this.length, this.unknown)
    }
  }

  private case class CandidateParse(parse: ParsedChunk, curTrie: List[KoreanPosTrie],
      ending: Option[KoreanPos])

  private case class PossibleTrie(curTrie: KoreanPosTrie, words: Int)

  def printTrie(trie: KoreanPosTrie, emptyStr:String=""):Unit = {
    println(s"$emptyStr curPos: ${trie.curPos}, ending: ${trie.ending}")
    if (trie.nextTrie != null) trie.nextTrie.map(tr=>printTrie(tr, emptyStr + "  "))
  }


}