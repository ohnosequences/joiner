
```scala
package ohnosequences

import spire.implicits._

/**
 * This package exposes functions to work with sequences and joint probability
 * distributions.
 */
package object joiner {

  type DNASeq = Array[DNADistributions.DNAD]

  /**
   * Convert a Phred quality score to an error probability
   */
  def scoreToErrorProb(n: Int): Double =
    Math.max(
      consensus.minErr,
      (10:Double) fpow ( - ((n:Double) / 10) )
    )

  /**
   * Convert an error probability to a Phred quality score
   */
  def errorProbToScore(err: Double): Int =
    Math.min(
      consensus.maxQScore,
      Math.round((-10:Double) * err.log(10)).toInt
    )
}

```




[main/scala/intervals.scala]: intervals.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/bestOverlap.scala]: bestOverlap.scala.md
[main/scala/DNADistributions.scala]: DNADistributions.scala.md
[main/scala/consensus.scala]: consensus.scala.md
[test/scala/Intervals.scala]: ../../test/scala/Intervals.scala.md
[test/scala/BestOverlap.scala]: ../../test/scala/BestOverlap.scala.md
[test/scala/Joining.scala]: ../../test/scala/Joining.scala.md
[test/scala/Joiner.scala]: ../../test/scala/Joiner.scala.md
[test/scala/Consensus.scala]: ../../test/scala/Consensus.scala.md