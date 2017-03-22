SBT
===

-	build: `$ sbt package -verbose`
-	test: `$ sbt test`
-	run
	-	**사전 업데이트 등의 작업 후에 할 것** &mdash; `$ sbt "runMain com.twitter.penguin.korean.tools.UpdateAllTheExamples"`
	-	기타
        - `$ sbt "runMain Runner"`
		-	`$ sbt "runMain com.twitter.penguin.korean.qa.BatchGetUnknownNouns ./src/main/resources/com/twitter/penguin/korean/util/example_tweets.txt"`
		-	`$ sbt "runMain com.twitter.penguin.korean.qa.BatchGetUnknownNouns ./src/main/resources/com/twitter/penguin/korean/util/example_tweets.txt"`
        - `$ sbt "runMain com.quryon.qa.BatchTokenizeSentences ./src/main/resources/com/twitter/penguin/korean/util/example_sentences.txt"`