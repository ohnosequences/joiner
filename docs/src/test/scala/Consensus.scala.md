
```scala
package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.fastarious._
import ohnosequences.joiner._, DNADistributions._, consensus._

class Consensus extends FunSuite {

  test("error for same bases") {

    val err: Double =
      0.1D

    val qs =
      PSymbol('A', err)

    val d =
      pSymbolToDNAD(qs)

    val qss =
      Seq.fill(100)(qs)

    val dnaseq =
      qss map pSymbolToDNAD

    val joinScore =
      qss.map(_.toQSymbol)
        .foldLeft(errorProbToScore(0.75)){
          (acc, qs) => jointScoreSameBase(qs.score, acc)
        }

    val join2 =
      (dnaseq.foldLeft(uniform){ join _ })

    println(s"     joint Score: ${joinScore}")
    println(s"     joint error: ${scoreToErrorProb(joinScore)}")
    println(s"      joint DNAD: ${join2}")
    println(s"joint DNAD error: ${join2.errorProb}")

    // println(Seq.tabulate(10000){ n: Int => io.scoreToErrorProb(n) }.zipWithIndex)
  }
}

```




[main/scala/intervals.scala]: ../../main/scala/intervals.scala.md
[main/scala/package.scala]: ../../main/scala/package.scala.md
[main/scala/bestOverlap.scala]: ../../main/scala/bestOverlap.scala.md
[main/scala/DNADistributions.scala]: ../../main/scala/DNADistributions.scala.md
[main/scala/consensus.scala]: ../../main/scala/consensus.scala.md
[test/scala/Intervals.scala]: Intervals.scala.md
[test/scala/BestOverlap.scala]: BestOverlap.scala.md
[test/scala/Joining.scala]: Joining.scala.md
[test/scala/Joiner.scala]: Joiner.scala.md
[test/scala/Consensus.scala]: Consensus.scala.md