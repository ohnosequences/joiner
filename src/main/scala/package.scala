package ohnosequences

import spire.implicits._

package object joiner {

  type DNASeq = Array[DNADistributions.DNAD]

  def scoreToErrorProb(n: Int): Double =
    Math.max(
      consensus.minErr,
      (10:Double) fpow ( - ((n:Double) / 10) )
    )

  def errorProbToScore(err: Double): Int =
    Math.min(
      consensus.maxQScore,
      Math.round((-10:Double) * err.log(10)).toInt
    )
}
