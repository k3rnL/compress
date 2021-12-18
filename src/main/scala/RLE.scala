package compress

/** The Run-length encoding compression method */
class RLE[T] extends Compressor[T, Seq[(T, Int)]] {

  /** @inheritdoc */
  def compress(msg: Seq[T]): Seq[(T, Int)] = msg match {
    case Nil => Nil
    case value +: restMsg => compress(restMsg) match {
      case (`value`, count) +: rest => (value, count + 1) +: rest
      case rest => (value, 1) +: rest
    }
  }

  /** @inheritdoc */
  def uncompress(seq: Seq[(T, Int)]): Option[Seq[T]] = seq match {
    case Nil => None
    case compressed =>
      if (compressed.map(_._2).min < 1) None
      else Some(compressed.map(e => repeat(e._1, e._2)).reduce((a, b) => a ++ b))
  }

  def repeat(e: T, count: Int): Seq[T] = Seq.fill(count)(e)
}
