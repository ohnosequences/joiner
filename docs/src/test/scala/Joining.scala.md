
```scala
package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.joiner._, DNADistributions._, intervals._, bestOverlap._

class Joining extends FunSuite {


  val A     = DNAD(A = 0.99, T = 0.005, C = 0.0025, G = 0.0025)
  val lowA  = DNAD(A = 0.7, T = 0.1, C = 0.1, G = 0.1)
  val T     = DNAD(T = 0.99, A = 0.005, C = 0.0025, G = 0.0025)

  test("conservative substitution to uniform") {

    val zz: DNASeq = Array(A,lowA,T,lowA,T)

    assert { zz.uniformOver(0.2)  === Array(A,uniform,T,uniform,T) }
    assert { zz.uniformOver(0.31) === zz }
    assert { Array(uniform,uniform,uniform).uniformOver(0.3) === Array(uniform,uniform,uniform) }
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