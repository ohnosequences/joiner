package ohnosequences.joiner

import ohnosequences.fastarious._

case object DNADistributions {

  case class DNAD(
    val A: Prob,
    val T: Prob,
    val C: Prob,
    val G: Prob
  )
  {
    def apply(c: Char): Prob =
      c match {
        case 'A' => A
        case 'T' => T
        case 'C' => C
        case 'G' => G
        case 'N' => 1
      }

    private[this] val sortedBases: Array[Char] =
      Array[Char]('A','T','C','G').sortBy(1 - this(_))

    def mostLikely: Char =
      sortedBases.head

    def errorProb: Prob = 1 - this(mostLikely)
  }

  val uniform: DNAD =
    DNAD(A = 0.25, T = 0.25, C = 0.25, G = 0.25)

  val fromPSymbol: PSymbol => DNAD =
    {
      case PSymbol('A', err) => DNAD(A = 1 - err, T = err/3, C = err/3, G = err/3)
      case PSymbol('T', err) => DNAD(T = 1 - err, A = err/3, C = err/3, G = err/3)
      case PSymbol('C', err) => DNAD(C = 1 - err, T = err/3, A = err/3, G = err/3)
      case PSymbol('G', err) => DNAD(G = 1 - err, T = err/3, C = err/3, A = err/3)
      case _                 => uniform
    }

  val pSymbolsToDNADs: Seq[PSymbol] => Array[DNAD] =
    qss => {

      val ds: Array[DNAD] = Array.fill(qss.length)(null)

      var i = 0
      while(i < ds.length) {
        ds.update(i, fromPSymbol(qss(i)))
        i = i + 1
      }

      ds
    }

  def deltaProb(d1: DNAD, d2: DNAD): Prob =
    (d1.A * d2.A) + (d1.T * d2.T) + (d1.C * d2.C) + (d1.G * d2.G)

  def join(d1: DNAD, d2: DNAD): DNAD =
    DNAD(
      A = (d1.A * d2.A) / deltaProb(d1,d2),
      T = (d1.T * d2.T) / deltaProb(d1,d2),
      C = (d1.C * d2.C) / deltaProb(d1,d2),
      G = (d1.G * d2.G) / deltaProb(d1,d2)
    )

  implicit class ConsensusOps(val dsds: Array[Array[DNAD]]) extends AnyVal {

    // compute the join of distributions at each column
    def consensus(len: Int): Array[DNAD] = {

      def read(i: Int) = dsds(i)

      val cons = Array.fill[DNAD](len)(uniform)
      var i   = 0 // read
      var pos = 0 // position

      // iter reads
      while(pos < len) {
        while(i < dsds.length) {
          var pos_res = uniform
          while(i < dsds.length) {
            pos_res = join( pos_res, read(i)(pos) )
            i = i + 1
          }
          cons.update(pos, pos_res)
        }
        i = 0
        pos = pos + 1
      }
      cons
    }
  }

  implicit class DNADOps(val ds: Array[DNAD]) extends AnyVal {

    def show: String =
      ds.foldLeft(""){ (acc, d) => acc ++ s"${d.mostLikely}:${d.errorProb}|" }

    def joinAll: DNAD = {

      var i   = 0
      var d   = uniform
      var res = d

      while(i < ds.length) { res = join(d,res); i = i + 1 }

      res
    }
    // ds.foldLeft(uniform)(join)

    def ee: Double = {

      var sum = 0D
      var i = 0

      while(i < ds.length) {
        sum = ds(i).errorProb + sum
        i = i + 1
      }

      sum
    }
  }
}
