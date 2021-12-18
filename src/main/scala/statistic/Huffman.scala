package compress.statistic

import scala.annotation.tailrec

/** The HUFFMAN compression method */
class Huffman[S](source: Seq[S]) extends StatisticCompressor[S](source) {
  def mergeNode(left: EncodingTree[S], right: EncodingTree[S]): EncodingTree[S] =
    EncodingNode(left.label + right.label, left, right)

  @tailrec
  final def build(queue: Seq[EncodingTree[S]]): EncodingTree[S] = queue match {
    case result +: Nil => result
    case first +: second +: Nil => mergeNode(first, second)
    case first +: second +: rest => build((rest :+ mergeNode(first, second)).sortBy(_.label))
  }

  /** @inheritdoc */
  lazy val tree: Option[EncodingTree[S]] = source match {
    case Nil => None
    case _ => Some(build(orderedCounts.map(f => EncodingLeaf(f._2, f._1))))
  }
}


