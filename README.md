`Trampoline[_]`
===============

Working examples for slides at
https://slides.com/seigert/eat

To run this examples you'll need [`scala-cli`]:

```console
$> scala-cli run project.scala 3.1.eval.sc
Compiling project (Scala 3.2.1, JVM)
Compiled project (Scala 3.2.1, JVM)
3.1.eval.isOdd(100501) = Eval.FlatMap(.., .. => ..)
3.1.eval.isEven(100501) = Eval.FlatMap(.., .. => ..)
3.1.eval.odd.result = Right(true)
3.1.eval.even.result = Right(false)
3.1.eval.hasEvenEval(scala.LazyList.continually[scala.Int](scala.util.Random.nextInt(1024))).result = Right(true)
3.1.eval.hasEvenFold(scala.LazyList.continually[scala.Int](scala.util.Random.nextInt(1024))).result = Right(true)
```

[`scala-cli`]: https://scala-cli.virtuslab.org/
