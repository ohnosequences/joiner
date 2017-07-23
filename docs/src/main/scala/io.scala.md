
```scala
package ohnosequences.joiner

import DNADistributions._
import spire.implicits._
import ohnosequences.fastarious._

case object io {

  def scoreToErrorProb(n: Int) = (10:Double) fpow ( - ((n:Double) / 10) )

  implicit class FASTQLines(val lines: Iterator[String]) extends AnyVal {

    def parseFromFASTQ: Iterator[DNASeq] =
      lines.zipWithIndex
        .collect({ case (x,i) if(i % 2 == 1) => x })
        .grouped(2).filter(_.size == 2) // grouped is broken
        .map { quartet =>
          val chars   = quartet(0)
          val scores  = quartet(1)
          val ds = Array.fill[DNAD](chars.length)(uniform)
          var i = 0
          while(i < ds.length) {
            ds.update( i, fromPSymbol(PSymbol(chars(i),scoreToErrorProb(scores(i).toInt))) )
            i = i+1
          }
          ds
        }

    def parseFromFASTQReuseArray(len: Int): Iterator[DNASeq] = new Iterator[DNASeq] {

      val dnaseq = Array.fill[DNAD](len)(uniform)

      val charsScores =
        lines.zipWithIndex
          .collect({ case (x,i) if(i % 2 == 1) => x })
          .grouped(2).filter(_.size == 2) // grouped is broken
          .map { quartet =>
            quartet(0) -> quartet(1)
          }

      def hasNext =
        charsScores.hasNext

      def next = {

        val (chars, scores) = charsScores.next

        var i = 0
        while(i < dnaseq.length) {
          dnaseq.update( i, fromPSymbol(PSymbol(chars(i),scoreToErrorProb(scores(i).toInt))) )
          i = i+1
        }
        dnaseq
      }
    }
  }
}

```




[test/scala/BestOverlap.scala]: ../../test/scala/BestOverlap.scala.md
[test/scala/Intervals.scala]: ../../test/scala/Intervals.scala.md
[test/scala/Joiner.scala]: ../../test/scala/Joiner.scala.md
[main/scala/DNADistributions.scala]: DNADistributions.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/intervals.scala]: intervals.scala.md
[main/scala/io.scala]: io.scala.md
[main/scala/bestOverlap.scala]: bestOverlap.scala.md