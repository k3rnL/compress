package compress.lz

import compress.Compressor
import Dictionaries._

import scala.annotation.tailrec

/** The LZW compression method
  *
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary: Dictionary = ASCII) extends Compressor[Char, Seq[Int]] {

  @tailrec
  final def eat(msg: Seq[Char], toEat: Int, dict: Dictionary): Option[(Seq[Char], Int)] = msg.take(toEat) match {
    case Nil => None
    case a if a.length < toEat => Some(msg, toEat)
    case a if dict.contains(a.mkString) => eat(msg, toEat + 1, dict)
    case a => Some(a, toEat)
  }

  @tailrec
  final def runCompress(msg: Seq[Char], result: Seq[(Int, Char)], dict: Dictionary): Seq[(Int, Char)] = eat(msg, 1, dict) match {
    case None => result
    case Some((eatenValue, size)) => eatenValue match {
      case `msg` => result :+ (dict.indexOf(msg.mkString), msg.last)
      case existingValue :+ newValue => runCompress(msg.drop(size - 1), result :+ (dict.indexOf(existingValue.mkString), newValue), dict :+ eatenValue.mkString)
    }
  }

  /** @inheritdoc */
  def compress(msg: Seq[Char]): Seq[Int] = runCompress(msg, Seq(), Dictionaries.ASCII).map(_._1)

  @tailrec
  final def uncompress(input: Seq[Int], result: Seq[Char], accum: Seq[Char], dict: Dictionary): Option[Seq[Char]] = input match {
    case Nil => Some(result ++ accum)
    case head +: tail => if (dict.indices.contains(head)) {
      uncompress(tail, result ++ accum, dict(head), dict :+ (accum ++ dict(head)).mkString)
    } else {
      if (head == dict.length)
        uncompress(tail, result ++ accum, Seq(accum.last), dict)
      else None
    }
  }

  /** @inheritdoc */
  def uncompress(res: Seq[Int]): Option[Seq[Char]] = res match {
    case Nil => Some(Seq())
    case res => uncompress(res.takeRight(res.length - 1), Seq(), Dictionaries.ASCII(res.head).toSeq, Dictionaries.ASCII)
  }
}


