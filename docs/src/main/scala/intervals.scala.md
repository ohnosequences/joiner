
```scala
package ohnosequences.joiner

case object intervals {

  type Nat = Int

  def isNat(n: Int): Boolean =
    n >= 0

  def interval(s: Int, e: Int): Interval =
    if(s >= e)
      Empty
    else
      if(isNat(s) && isNat(e))
        LCRO(s, e)
      else
        interval(Math.max(0,s), Math.max(0,e))

  sealed trait Interval {
    def start : Nat
    def end   : Nat

    def before: Interval =
      interval(0, start)

    def after: Interval =
      interval(end, Int.MaxValue)

    def isEmpty: Boolean = end <= start
    def length = end - start
  }
  // [start, end[ Left Closed Right Open
  case class LCRO(start: Nat, end: Nat) extends Interval {

    override def toString =
      s"[${start},${end}["
  }
  case object Empty extends Interval {
    def start = 0
    def end = 0

    override def toString = "∅"
  }

  // then overlap at pos between xs, ys yields 3 intervals
  // xs_before, xs_overlap, ys_overlap, ys_after
  case class OverlapHC(
    prefix  : Interval,
    overlap : Interval,
    suffix  : Interval
  ) {

    def firstLength =
      prefix.length + overlap.length

    def secondLength =
      suffix.length + overlap.length
  }

  case class RelativeOverlap(
    firstPrefix   : Interval,
    firstOverlap  : Interval,
    secondOverlap : Interval,
    secondSuffix  : Interval
  )

  def disjoint(i1: Interval, i2: Interval): Boolean =
    i1.end <= i2.start

  def overlapping(i1: Interval, i2: Interval): Boolean =
    i1.start < i2.end && i2.start < i1.end

  def whole[X](seq: Array[X]): Interval =
    interval(0, seq.length)

  def translate(add: Int)(i: Interval): Interval =
    interval(i.start + add, i.end + add)

  def intersection(i1: Interval, i2: Interval): Interval =
    interval(Math.max(i1.start, i2.start), Math.min(i1.end, i2.end))

  def overlap(first: Interval, second: Interval): OverlapHC = {

    val shared = intersection(first,second)

    OverlapHC(
      intersection(first, shared.before),
      shared,
      intersection(second, shared.after)
    )
  }

  // (before, both_s1, both_s2, after) where
  // before is a subinterval of s1
  //
  def overlapsWithLength(length: Int)(s1: Interval): Array[(Interval,Interval, Interval, Interval)] =
    Array.tabulate(s1.length)({ i =>
      val shared = interval(s1.start + i, s1.start + i + length)
      (
        intersection(s1, shared.before),
        intersection(s1, shared),
        translate(-i)(intersection(s1,shared)),
        translate(-i)(intersection(s1.after, shared))
      )
    })
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