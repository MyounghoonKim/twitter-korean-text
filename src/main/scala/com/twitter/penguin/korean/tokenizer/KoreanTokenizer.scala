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

import java.io._
import sys.process._

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
  private val SequenceDefinition = Map(
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

  def tokenize(text: CharSequence,
      profile: TokenizerProfile = TokenizerProfile.defaultProfile,
      makeAnalysisDoc: Boolean = false
  ): Seq[KoreanToken] = {
    val pw = if (makeAnalysisDoc) {
      val base = s"""<!DOCTYPE html><html><head><meta charset="utf-8"><style>@font-face {
          font-family: octicons-anchor;
          src: url(data:font/woff;charset=utf-8;base64,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) format('woff');
        }

        * {
            box-sizing: border-box;
        }

        body {
            width: 980px;
            margin-right: auto;
            margin-left: auto;
        }

        body .markdown-body {
            padding: 45px;
            border: 1px solid #ddd;
            border-radius: 3px;
            word-wrap: break-word;
        }

        pre {
            font: 12px Consolas, "Liberation Mono", Menlo, Courier, monospace;
        }

        .markdown-body {
          -webkit-text-size-adjust: 100%;
          text-size-adjust: 100%;
          color: #333;
          font-family: "Helvetica Neue", Helvetica, "Segoe UI", Arial, freesans, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
          font-size: 16px;
          line-height: 1.6;
          word-wrap: break-word;
        }

        .markdown-body a {
          background-color: transparent;
        }

        .markdown-body a:active,
        .markdown-body a:hover {
          outline: 0;
        }

        .markdown-body strong {
          font-weight: bold;
        }

        .markdown-body h1 {
          font-size: 2em;
          margin: 0.67em 0;
        }

        .markdown-body img {
          border: 0;
        }

        .markdown-body hr {
          box-sizing: content-box;
          height: 0;
        }

        .markdown-body pre {
          overflow: auto;
        }

        .markdown-body code,
        .markdown-body kbd,
        .markdown-body pre {
          font-family: monospace, monospace;
          font-size: 1em;
        }

        .markdown-body input {
          color: inherit;
          font: inherit;
          margin: 0;
        }

        .markdown-body html input[disabled] {
          cursor: default;
        }

        .markdown-body input {
          line-height: normal;
        }

        .markdown-body input[type="checkbox"] {
          box-sizing: border-box;
          padding: 0;
        }

        .markdown-body table {
          border-collapse: collapse;
          border-spacing: 0;
        }

        .markdown-body td,
        .markdown-body th {
          padding: 0;
        }

        .markdown-body input {
          font: 13px / 1.4 Helvetica, arial, nimbussansl, liberationsans, freesans, clean, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
        }

        .markdown-body a {
          color: #4078c0;
          text-decoration: none;
        }

        .markdown-body a:hover,
        .markdown-body a:active {
          text-decoration: underline;
        }

        .markdown-body hr {
          height: 0;
          margin: 15px 0;
          overflow: hidden;
          background: transparent;
          border: 0;
          border-bottom: 1px solid #ddd;
        }

        .markdown-body hr:before {
          display: table;
          content: "";
        }

        .markdown-body hr:after {
          display: table;
          clear: both;
          content: "";
        }

        .markdown-body h1,
        .markdown-body h2,
        .markdown-body h3,
        .markdown-body h4,
        .markdown-body h5,
        .markdown-body h6 {
          margin-top: 15px;
          margin-bottom: 15px;
          line-height: 1.1;
        }

        .markdown-body h1 {
          font-size: 30px;
        }

        .markdown-body h2 {
          font-size: 21px;
        }

        .markdown-body h3 {
          font-size: 16px;
        }

        .markdown-body h4 {
          font-size: 14px;
        }

        .markdown-body h5 {
          font-size: 12px;
        }

        .markdown-body h6 {
          font-size: 11px;
        }

        .markdown-body blockquote {
          margin: 0;
        }

        .markdown-body ul,
        .markdown-body ol {
          padding: 0;
          margin-top: 0;
          margin-bottom: 0;
        }

        .markdown-body ol ol,
        .markdown-body ul ol {
          list-style-type: lower-roman;
        }

        .markdown-body ul ul ol,
        .markdown-body ul ol ol,
        .markdown-body ol ul ol,
        .markdown-body ol ol ol {
          list-style-type: lower-alpha;
        }

        .markdown-body dd {
          margin-left: 0;
        }

        .markdown-body code {
          font-family: Consolas, "Liberation Mono", Menlo, Courier, monospace;
          font-size: 12px;
        }

        .markdown-body pre {
          margin-top: 0;
          margin-bottom: 0;
          font: 12px Consolas, "Liberation Mono", Menlo, Courier, monospace;
        }

        .markdown-body .select::-ms-expand {
          opacity: 0;
        }

        .markdown-body .octicon {
          font: normal normal normal 16px/1 octicons-anchor;
          display: inline-block;
          text-decoration: none;
          text-rendering: auto;
          -webkit-font-smoothing: antialiased;
          -moz-osx-font-smoothing: grayscale;
          -webkit-user-select: none;
          -moz-user-select: none;
          -ms-user-select: none;
          user-select: none;
        }

        .markdown-body .octicon-link:before {
          content: '\f05c';
        }

        .markdown-body:before {
          display: table;
          content: "";
        }

        .markdown-body:after {
          display: table;
          clear: both;
          content: "";
        }

        .markdown-body>*:first-child {
          margin-top: 0 !important;
        }

        .markdown-body>*:last-child {
          margin-bottom: 0 !important;
        }

        .markdown-body a:not([href]) {
          color: inherit;
          text-decoration: none;
        }

        .markdown-body .anchor {
          display: inline-block;
          padding-right: 2px;
          margin-left: -18px;
        }

        .markdown-body .anchor:focus {
          outline: none;
        }

        .markdown-body h1,
        .markdown-body h2,
        .markdown-body h3,
        .markdown-body h4,
        .markdown-body h5,
        .markdown-body h6 {
          margin-top: 1em;
          margin-bottom: 16px;
          font-weight: bold;
          line-height: 1.4;
        }

        .markdown-body h1 .octicon-link,
        .markdown-body h2 .octicon-link,
        .markdown-body h3 .octicon-link,
        .markdown-body h4 .octicon-link,
        .markdown-body h5 .octicon-link,
        .markdown-body h6 .octicon-link {
          color: #000;
          vertical-align: middle;
          visibility: hidden;
        }

        .markdown-body h1:hover .anchor,
        .markdown-body h2:hover .anchor,
        .markdown-body h3:hover .anchor,
        .markdown-body h4:hover .anchor,
        .markdown-body h5:hover .anchor,
        .markdown-body h6:hover .anchor {
          text-decoration: none;
        }

        .markdown-body h1:hover .anchor .octicon-link,
        .markdown-body h2:hover .anchor .octicon-link,
        .markdown-body h3:hover .anchor .octicon-link,
        .markdown-body h4:hover .anchor .octicon-link,
        .markdown-body h5:hover .anchor .octicon-link,
        .markdown-body h6:hover .anchor .octicon-link {
          visibility: visible;
        }

        .markdown-body h1 {
          padding-bottom: 0.3em;
          font-size: 2.25em;
          line-height: 1.2;
          border-bottom: 1px solid #eee;
        }

        .markdown-body h1 .anchor {
          line-height: 1;
        }

        .markdown-body h2 {
          padding-bottom: 0.3em;
          font-size: 1.75em;
          line-height: 1.225;
          border-bottom: 1px solid #eee;
        }

        .markdown-body h2 .anchor {
          line-height: 1;
        }

        .markdown-body h3 {
          font-size: 1.5em;
          line-height: 1.43;
        }

        .markdown-body h3 .anchor {
          line-height: 1.2;
        }

        .markdown-body h4 {
          font-size: 1.25em;
        }

        .markdown-body h4 .anchor {
          line-height: 1.2;
        }

        .markdown-body h5 {
          font-size: 1em;
        }

        .markdown-body h5 .anchor {
          line-height: 1.1;
        }

        .markdown-body h6 {
          font-size: 1em;
          color: #777;
        }

        .markdown-body h6 .anchor {
          line-height: 1.1;
        }

        .markdown-body p,
        .markdown-body blockquote,
        .markdown-body ul,
        .markdown-body ol,
        .markdown-body dl,
        .markdown-body table,
        .markdown-body pre {
          margin-top: 0;
          margin-bottom: 16px;
        }

        .markdown-body hr {
          height: 4px;
          padding: 0;
          margin: 16px 0;
          background-color: #e7e7e7;
          border: 0 none;
        }

        .markdown-body ul,
        .markdown-body ol {
          padding-left: 2em;
        }

        .markdown-body ul ul,
        .markdown-body ul ol,
        .markdown-body ol ol,
        .markdown-body ol ul {
          margin-top: 0;
          margin-bottom: 0;
        }

        .markdown-body li>p {
          margin-top: 16px;
        }

        .markdown-body dl {
          padding: 0;
        }

        .markdown-body dl dt {
          padding: 0;
          margin-top: 16px;
          font-size: 1em;
          font-style: italic;
          font-weight: bold;
        }

        .markdown-body dl dd {
          padding: 0 16px;
          margin-bottom: 16px;
        }

        .markdown-body blockquote {
          padding: 0 15px;
          color: #777;
          border-left: 4px solid #ddd;
        }

        .markdown-body blockquote>:first-child {
          margin-top: 0;
        }

        .markdown-body blockquote>:last-child {
          margin-bottom: 0;
        }

        .markdown-body table {
          display: block;
          width: 100%;
          overflow: auto;
          word-break: normal;
          word-break: keep-all;
        }

        .markdown-body table th {
          font-weight: bold;
        }

        .markdown-body table th,
        .markdown-body table td {
          padding: 6px 13px;
          border: 1px solid #ddd;
        }

        .markdown-body table tr {
          background-color: #fff;
          border-top: 1px solid #ccc;
        }

        .markdown-body table tr:nth-child(2n) {
          background-color: #f8f8f8;
        }

        .markdown-body img {
          max-width: 100%;
          box-sizing: content-box;
          background-color: #fff;
        }

        .markdown-body code {
          padding: 0;
          padding-top: 0.2em;
          padding-bottom: 0.2em;
          margin: 0;
          font-size: 85%;
          background-color: rgba(0,0,0,0.04);
          border-radius: 3px;
        }

        .markdown-body code:before,
        .markdown-body code:after {
          letter-spacing: -0.2em;
          content: "\00a0";
        }

        .markdown-body pre>code {
          padding: 0;
          margin: 0;
          font-size: 100%;
          word-break: normal;
          white-space: pre;
          background: transparent;
          border: 0;
        }

        .markdown-body .highlight {
          margin-bottom: 16px;
        }

        .markdown-body .highlight pre,
        .markdown-body pre {
          padding: 16px;
          overflow: auto;
          font-size: 85%;
          line-height: 1.45;
          background-color: #f7f7f7;
          border-radius: 3px;
        }

        .markdown-body .highlight pre {
          margin-bottom: 0;
          word-break: normal;
        }

        .markdown-body pre {
          word-wrap: normal;
        }

        .markdown-body pre code {
          display: inline;
          max-width: initial;
          padding: 0;
          margin: 0;
          overflow: initial;
          line-height: inherit;
          word-wrap: normal;
          background-color: transparent;
          border: 0;
        }

        .markdown-body pre code:before,
        .markdown-body pre code:after {
          content: normal;
        }

        .markdown-body kbd {
          display: inline-block;
          padding: 3px 5px;
          font-size: 11px;
          line-height: 10px;
          color: #555;
          vertical-align: middle;
          background-color: #fcfcfc;
          border: solid 1px #ccc;
          border-bottom-color: #bbb;
          border-radius: 3px;
          box-shadow: inset 0 -1px 0 #bbb;
        }

        .markdown-body .pl-c {
          color: #969896;
        }

        .markdown-body .pl-c1,
        .markdown-body .pl-s .pl-v {
          color: #0086b3;
        }

        .markdown-body .pl-e,
        .markdown-body .pl-en {
          color: #795da3;
        }

        .markdown-body .pl-s .pl-s1,
        .markdown-body .pl-smi {
          color: #333;
        }

        .markdown-body .pl-ent {
          color: #63a35c;
        }

        .markdown-body .pl-k {
          color: #a71d5d;
        }

        .markdown-body .pl-pds,
        .markdown-body .pl-s,
        .markdown-body .pl-s .pl-pse .pl-s1,
        .markdown-body .pl-sr,
        .markdown-body .pl-sr .pl-cce,
        .markdown-body .pl-sr .pl-sra,
        .markdown-body .pl-sr .pl-sre {
          color: #183691;
        }

        .markdown-body .pl-v {
          color: #ed6a43;
        }

        .markdown-body .pl-id {
          color: #b52a1d;
        }

        .markdown-body .pl-ii {
          background-color: #b52a1d;
          color: #f8f8f8;
        }

        .markdown-body .pl-sr .pl-cce {
          color: #63a35c;
          font-weight: bold;
        }

        .markdown-body .pl-ml {
          color: #693a17;
        }

        .markdown-body .pl-mh,
        .markdown-body .pl-mh .pl-en,
        .markdown-body .pl-ms {
          color: #1d3e81;
          font-weight: bold;
        }

        .markdown-body .pl-mq {
          color: #008080;
        }

        .markdown-body .pl-mi {
          color: #333;
          font-style: italic;
        }

        .markdown-body .pl-mb {
          color: #333;
          font-weight: bold;
        }

        .markdown-body .pl-md {
          background-color: #ffecec;
          color: #bd2c00;
        }

        .markdown-body .pl-mi1 {
          background-color: #eaffea;
          color: #55a532;
        }

        .markdown-body .pl-mdr {
          color: #795da3;
          font-weight: bold;
        }

        .markdown-body .pl-mo {
          color: #1d3e81;
        }

        .markdown-body kbd {
          display: inline-block;
          padding: 3px 5px;
          font: 11px Consolas, "Liberation Mono", Menlo, Courier, monospace;
          line-height: 10px;
          color: #555;
          vertical-align: middle;
          background-color: #fcfcfc;
          border: solid 1px #ccc;
          border-bottom-color: #bbb;
          border-radius: 3px;
          box-shadow: inset 0 -1px 0 #bbb;
        }

        .markdown-body .plan-price-unit {
          color: #767676;
          font-weight: normal;
        }

        .markdown-body .task-list-item {
          list-style-type: none;
        }

        .markdown-body .task-list-item+.task-list-item {
          margin-top: 3px;
        }

        .markdown-body .task-list-item input {
          margin: 0 0.35em 0.25em -1.6em;
          vertical-align: middle;
        }

        .markdown-body .plan-choice {
          padding: 15px;
          padding-left: 40px;
          display: block;
          border: 1px solid #e0e0e0;
          position: relative;
          font-weight: normal;
          background-color: #fafafa;
        }

        .markdown-body .plan-choice.open {
          background-color: #fff;
        }

        .markdown-body .plan-choice.open .plan-choice-seat-breakdown {
          display: block;
        }

        .markdown-body .plan-choice-free {
          border-radius: 3px 3px 0 0;
        }

        .markdown-body .plan-choice-paid {
          border-radius: 0 0 3px 3px;
          border-top: 0;
          margin-bottom: 20px;
        }

        .markdown-body .plan-choice-radio {
          position: absolute;
          left: 15px;
          top: 18px;
        }

        .markdown-body .plan-choice-exp {
          color: #999;
          font-size: 12px;
          margin-top: 5px;
        }

        .markdown-body .plan-choice-seat-breakdown {
          margin-top: 10px;
          display: none;
        }

        .markdown-body :checked+.radio-label {
          z-index: 1;
          position: relative;
          border-color: #4078c0;
        }
        </style><title>analysis-${text}</title><body><article class="markdown-body">
        """


      // val currentTime = System.currentTimeMillis().toString
      // just for text
      val currentTime = "123123"
      s"mkdir -p analysis-results/${currentTime}".!
      val printWriter = new PrintWriter(new File(s"analysis-results/${currentTime}/" + text + ".html" ))
      printWriter.write(base)
      printWriter.write("")
      printWriter
    }
    val result = try {
      chunk(text).flatMap {
        case token: KoreanToken if token.pos == Korean =>
          // Get the best parse of each chunk
          val parsed = if (makeAnalysisDoc) {
            parseKoreanChunkWithAnalysisDoc(token, profile, pw.asInstanceOf[PrintWriter])
          }

          else {
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
    if (makeAnalysisDoc) {
      pw.asInstanceOf[PrintWriter].close
    }

    return result
  }

  /**
    * Find the best parse using dynamic programming.
    *
    * @param chunk Input chunk. The input has to be entirely. Check for input validity is skipped
    *              for performance optimization. This method is private and is called only by tokenize.
    * @return The best possible parse.
    */
  private[this] def parseKoreanChunk(chunk: KoreanToken,
      profile: TokenizerProfile = TokenizerProfile.defaultProfile): Seq[KoreanToken] = {
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

  private[this] def parseKoreanChunkWithAnalysisDoc(chunk: KoreanToken,
      profile: TokenizerProfile = TokenizerProfile.defaultProfile, pw: PrintWriter): Seq[KoreanToken] = {
    // Direct match
    // This may produce 하 -> PreEomi
    write("<h1>" + chunk.toString + "</h1>")
    koreanDictionary.foreach {
      case (pos, dict) =>
        if (dict.contains(chunk.text)) {
          write(pos.toString + " 사전 안에 있음. 끝.\n")
          write("<h2>" + KoreanToken(chunk.text, pos, chunk.offset, chunk.length).toString + "</h2>")
          return Seq(KoreanToken(chunk.text, pos, chunk.offset, chunk.length))
        }
    }

    def write(str: String) = pw.write(str + "\n")

    // def makeTrieTable()


    // Buffer for solutions
    val solutions: mutable.Map[Int, List[CandidateParse]] = new java.util.HashMap[Int, List[CandidateParse]]

    // Initial state
    solutions += 0 -> List(
      CandidateParse(
        ParsedChunk(Seq[KoreanToken](), 1, profile),
        koreanPosTrie, ending = None
      )
    )

    for ((key, trees) <- solutions) {
      println(key)
      println(s"==trees.size: ${trees.size}")
      println(s"==trees.getClass: ${trees.getClass}")
      for (tree <- trees) {
        println(s"====tree.getClass:${tree.getClass}")
        println(s"====tree.ParsedChunk:${tree.parse}")
        println(s"====tree.curTrie.size:${tree.curTrie.size}")
      }
    }

    def printTrie(trie: KoreanPosTrie, emptyStr:String=""):Unit = {
      println(s"$emptyStr curPos: ${trie.curPos}, ending: ${trie.ending}")
      if (trie.nextTrie != null) trie.nextTrie.map(tr=>printTrie(tr, emptyStr + "  "))
    }
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
}