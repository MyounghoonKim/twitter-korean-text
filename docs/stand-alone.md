# how to run stand-alone parser
```shell
$ sbt assembly
$ scala target/scala-2.11/KoreanTextUtil-assembly-4.0.jar
```

# error handling
If you got this type of error, see http://stackoverflow.com/questions/40328948/java-lang-nosuchmethoderror-scala-predef-refarrayops/42919549#42919549
```shell
java.lang.NoSuchMethodError: scala.Predef$.refArrayOps([Ljava/lang/Object;)Lscala/collection/mutable/ArrayOps;
        at org.scalatest.tools.FriendlyParamsTranslator$.translateArguments(FriendlyParamsTranslator.scala:174)
        at org.scalatest.tools.Framework.runner(Framework.scala:918)
```
