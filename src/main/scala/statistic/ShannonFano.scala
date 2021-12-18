package compress.statistic

import scala.annotation.tailrec

/** The SHANNON-FANO compression method */
class ShannonFano[S](source: Seq[S]) extends StatisticCompressor[S](source) {
  @tailrec
  final def cutDistribution(left: Seq[(S, Int)], right: Seq[(S, Int)]): (Seq[(S, Int)], Seq[(S, Int)]) = (left.map(_._2).sum, right.map(_._2).sum) match {
    case (l, r) if l >= r || right.length == 1 => (left, right)
    case _ => cutDistribution(left :+ right.head, right.takeRight(right.length - 1))
  }

  def buildNode(left: EncodingTree[S], right: EncodingTree[S]): EncodingTree[S] =
    EncodingNode(left.label + right.label, left, right)

  def build(distribution: Seq[(S, Int)]): EncodingTree[S] = distribution match {
    case first +: Nil => EncodingLeaf(first._2, first._1)
    case remaining => cutDistribution(Seq(), remaining) match {
      case (left, right) => buildNode(build(left), build(right))
    }
  }

  /** @inheritdoc */
  lazy val tree: Option[EncodingTree[S]] = source match {
    case Nil => None
    case _ => Some(build(orderedCounts))
  }
}
