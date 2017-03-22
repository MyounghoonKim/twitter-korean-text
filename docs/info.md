# info

## pre-commit hook for git

```shell
echo "Running tests..."
sbt test
code=$?
echo $code

if [ "$code" -eq "0" ]; then
    echo
    echo
    echo "All tests passed. Continuing..."
    echo
else
    echo
    echo
    echo "Please (re)check tests!"
    exit $code
fi;
```

## test auto-run
```shell
$ sbt
sbt> ~test:test-only *ClassName*
fg) sbt> ~test:test-only *KoreanTokenizer*
```