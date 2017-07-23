package ohnosequences.joiner.test

import org.scalatest.FunSuite

import ohnosequences.joiner._, DNADistributions._, intervals._, bestOverlap._

class BestOverlap extends FunSuite {

  test("find best overlap") {

    val A = DNAD(A = 0.99, T = 0.005, C = 0.0025, G = 0.0025)
    val T = DNAD(T = 0.99, A = 0.005, C = 0.0025, G = 0.0025)
    val G = DNAD(G = 0.99, T = 0.005, C = 0.0025, A = 0.0025)

    val d1 = Array(T,T,T,T,G,G,G,G)
    val d2 =         Array(G,G,G,G,A,A,A,A,A,A)

    // should be [0,4[, [4,8[, [0,4[, [4,10[
    val (d,s) = bestOverlap.between(d1,d2)

    assert( d.map(_.mostLikely).toArray === "TTTTGGGGAAAAAA".toArray )
  }
}