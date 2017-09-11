package ohnosequences.joiner

import ohnosequences.fastarious._
import DNADistributions._

case object consensus {

  /*

    The input is an array of QSymbols. Then

    1. at each position group by letter and join them independently
    2. join the resulting 4 DNADs
  */

  lazy val maxQScore: Int =
    3000

  lazy val minErr: Double =
    QSymbol('A', maxQScore).toPSymbol.errorP

  def jointScoreSameBase(qscore1: Int, qscore2: Int): Int =
    Math.min(
      errorProbToScore(
        errSameBasePSymbols( scoreToErrorProb(qscore1), scoreToErrorProb(qscore2) )
      ),
      maxQScore
    )

  // compute the join of qsymbols directly
  // elements of this array corresponds to sequences
  def of(seqs: Array[Seq[QSymbol]])(len: Int): Array[DNAD] = {

    // uniform by default
    val cons =
      Array.fill[DNAD](len)(uniform)

    def joinSymbolAt(i: Int)(char: Symbol): DNAD = {

      var row = 0
      var errP = 0.75

      while(row < seqs.length) {

        val qs = seqs(row)(i)
        if(qs.symbol == char) { errP = errSameBasePSymbols(errP, scoreToErrorProb(qs.score)) }

        row = row + 1
      }

      val res =
        pSymbolToDNAD( QSymbol(char, errorProbToScore(errP)).toPSymbol )

      // println(s"${char} at ${i} error: ${errP}")
      // println(s" ${char} at ${i} DNAD: ${res}")

      res
    }

    def joinAllAt(i: Int): DNAD =
        join(
          join(
            joinSymbolAt(i)('A'),
            joinSymbolAt(i)('T')
          ),
          join(
            joinSymbolAt(i)('C'),
            joinSymbolAt(i)('G')
          )
        )

    var i = 0
    while(i < len) {
      cons.update(i, joinAllAt(i))
      i = i + 1
    }

    cons
  }

  def errSameBasePSymbols(err1: Double, err2: Double) = {

    val p1 = 1 - err1
    val p2 = 1 - err2

    val e =
      (err1 * err2)/3

    Math.max(
      minErr,
      e / (p1*p2 + e)
    )
  }



  def jointError(errs: Array[Double]): Double =
    errs.foldLeft(0.75D)(errSameBasePSymbols)
}
