package compress.statistic

/** Trait for binary encoding trees (integer-labeled binary trees)
  *
  * @tparam S type of symbols (in leaves only)
  */
sealed abstract class EncodingTree[S](val label: Int) {
  /* OPERATIONS ON VALUES */

  /** Checks if tree contains given value
    *
    * @param x value to search
    * @return true if the tree has a leaf with value `x`
    */
  def has(x: S): Boolean = this match {
    case EncodingLeaf(_, v) => v match {
      case x => true
      case any => false
    }
    case EncodingNode(_, left, right) => left.has(x) || right.has(x)
  }

  /** Reduce operation on tree values when applying a function on leaves beforehand
    *
    * @param f  the function applied to each leaf value
    * @param op the aggregation operation on a node
    * @tparam U the result type of `f`
    * @return the aggregated value of the tree
    */
  def reduceWith[U](f: S => U)(op: (U, U) => U): U = this match {
    case EncodingLeaf(_, v) => f(v)
    case EncodingNode(_, l, r) => op((l reduceWith f) (op), (r reduceWith f) (op))
  }

  /** Reduce operation on tree values
    *
    * `t reduce op` is a shorthand for `(t reduceWith {v => v})(op)`
    *
    * @param op the aggregation operation on a node
    * @return the aggregated value of the tree
    */
  def reduce(op: (S, S) => S): S = (this reduceWith { v => v }) (op)


  /* OPERATIONS ON LABELS */

  /** Reduce operation on tree labels when applying a function on leaves beforehand
    *
    * @param fL  the function applied to each leaf label
    * @param opL the aggregation operation on a node : first parameter is the node label
    * @tparam A the result type of `f`
    * @return the result of aggregation operation recursively applied to tree
    */
  def reduceLabelWith[A](fL: Int => A)(opL: (Int, A, A) => A): A = this match {
    case EncodingLeaf(lbl, _) => fL(lbl)
    case EncodingNode(lbl, l, r) => opL(lbl, (l reduceLabelWith fL) (opL), (r reduceLabelWith fL) (opL))
  }

  /** Reduce operation on tree labels
    *
    * `t reduceLabel opL` is a shorthand for `(t reduceLabelWith {lbl => lbl})(opL)`
    *
    * @param opL the aggregation operation on a node : first parameter is the node label
    * @return the aggregated label of the tree
    */
  def reduceLabel(opL: (Int, Int, Int) => Int): Int = (this reduceLabelWith identity) (opL)


  /* ENCODING/DECODING OPERATIONS */

  /** Computes the bit sequence corresponding to a tentative leaf value.
    *
    * @param x value to encode
    * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
    */
  def encode(x: S): Option[Seq[Bit]] = this match {
    case EncodingLeaf(label, value) => value match {
      case `x` => Some(Seq())
      case any => None
    }
    case EncodingNode(_, left, right) => left.encode(x) match {
      case Some(seq) => Some(Zero +: seq)
      case None => right.encode(x) match {
        case Some(seq) => Some(One +: seq)
        case None => None
      }
    }
  }

  /** Computes the next value corresponding to the beginning of bit sequence (if possible)
    *
    * @param res the bit sequence to decode
    * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in encoding tree
    */
  def decodeOnce(res: Seq[Bit]): Option[(S, Seq[Bit])] = this match {
    case EncodingLeaf(_, value) => Some(value, res)
    case EncodingNode(_, left, right) => res match {
      case Nil => None
      case head +: tail => head match {
        case One => right.decodeOnce(tail)
        case Zero => left.decodeOnce(tail)
      }
    }
  }

  /** Computes the sequence of values from the sequence of bits
    *
    * @param res the bit sequence to decode
    * @return the sequence of decoded values or `None` otherwise
    */
  def decode(res: Seq[Bit]): Option[Seq[S]] = res match {
    case Nil => None
    case res => decodeOnce(res) match {
      case None => None
      case Some((value, rest)) => Some(value +: decode(rest).getOrElse(Seq()))
    }
  }

  /* MISCELLANEOUS */

  /** Mean length of code associated to encoding tree */
  lazy val meanLength: Double =
    reduceLabelWith(l => Seq((0, l)))((_, l, r) => (l ++ r).map(e => (e._1 + 1, e._2)))
      .map(e => (e._1 * e._2, e._2))
      .reduce((l, r) => (l._1 + r._1, l._2 + r._2)) match {
      case (depthTotal, size) => depthTotal / size.toDouble
    }

  /** @inheritdoc */
  override def toString: String = this match {
    case EncodingLeaf(lbl, v) => (v, lbl).toString()
    case EncodingNode(lbl, l, r) => s"EncodingNode([$lbl], $l, $r)"
  }
}

case class EncodingNode[S](override val label: Int, left: EncodingTree[S], right: EncodingTree[S]) extends EncodingTree[S](label)

case class EncodingLeaf[S](override val label: Int, value: S) extends EncodingTree[S](label)

