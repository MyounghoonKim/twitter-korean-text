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

import com.twitter.penguin.korean.TestBase
import com.twitter.penguin.korean.tokenizer.KoreanTokenizer._
import com.twitter.penguin.korean.util.KoreanDictionaryProvider._
import com.twitter.penguin.korean.util.KoreanPos

class KoreanTokenizerQuryonTest extends TestBase {

 test("띄어쓰기") {
    println(tokenize("아버지가 방에 들어가신다").mkString(", "))
    println(tokenize("아버지가방에들어가신다").mkString(", "))
    println("아버지가 방에 들어가신다: 띄어쓰기를 하면 '가방'을 Noun에서 안 찾기 때문에 제대로 파싱됨")
  }

  test("도") {
    println(tokenize("포만감도 괜찮아요", verboseLevel=0).mkString(", "))
    addWordsToDictionary(KoreanPos.Noun, Seq("포만감"))
    println(tokenize("포만감도 괜찮아요", verboseLevel=0).mkString(", "))

    println(tokenize("면도 괜찮고", verboseLevel=1).mkString(", "))
  }
}