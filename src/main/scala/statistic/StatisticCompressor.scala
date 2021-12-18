package compress.statistic

import compress.Compressor

/** A statistic compressor relies on the statistics of symbols in source
  *
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source: Seq[S]) extends Compressor[S, Seq[Bit]] {
  /** A map giving the occurrences of each symbol in the source sequence */
  val occurrences: Map[S, Int] =
    source.foldRight(Map[S, Int]().withDefaultValue(0))((elem, acc) => acc + (elem -> (1 + acc(elem))))

  /** SHANNON entropy of source */
  val entropy: Double =
    occurrences.map(e => e._2 / source.length.toDouble).map(e => e * math.log(e) / math.log(2)).sum * -1

  /** The sequence of occurrences sorted by count */
  val orderedCounts: Seq[(S, Int)] = occurrences.toSeq.sortBy(_._2)

  /** The encoding tree (in most cases, depends from `source`) */
  def tree: Option[EncodingTree[S]]

  /** @inheritdoc */
  def compress(msg: Seq[S]): Seq[Bit] = tree match {
    case None => Seq()
    case Some(tree) => (for (e <- msg) yield tree.encode(e).get).flatten
  }

  /** @inheritdoc */
  def uncompress(res: Seq[Bit]): Option[Seq[S]] = tree match {
    case None => None
    case Some(tree) => tree.decode(res)
  }
}
