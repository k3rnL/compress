package compress.lz

import compress.Compressor
import compress.lz.Dictionaries.{Dictionary, empty}

import scala.annotation.tailrec

/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]] {
  @tailrec
  def eat(msg: Seq[Char], toEat: Int, dict: Dictionary): Option[(Seq[Char], Int)] = msg.take(toEat) match {
    case Nil => None
    case a if msg.length == 1 => Some(a, toEat)
    case a if dict.contains(a.mkString) => eat(msg, toEat + 1, dict)
    case a => Some(a, toEat)
  }

  @tailrec
  def runCompress(msg: Seq[Char], result: Seq[(Int, Char)], dict: Dictionary): Seq[(Int, Char)] = eat(msg, 1, dict) match {
    case None => result
    case Some((eatenValue, size)) => eatenValue match {
      case newValue +: Nil => runCompress(msg.drop(size), result :+ (0, newValue), dict :+ newValue.toString)
      case existingValue :+ newValue => runCompress(msg.drop(size), result :+ (dict.indexOf(existingValue.mkString), newValue), dict :+ eatenValue.mkString)
    }
  }

  /** @inheritdoc */
  def compress(msg: Seq[Char]): Seq[(Int, Char)] = runCompress(msg, Seq(), empty)

  def buildDict(input: (Int, Char), dict: Dictionary): Option[Dictionary] = input match {
    case (error, value) if !dict.indices.contains(error) => None
    case (0, value) => Some(dict :+ value.toString)
    case (index, value) => Some(dict :+ (dict(index) + value))
  }

  @tailrec
  def runUncompress(input: Seq[(Int, Char)], result: Seq[Char], dict: Dictionary): Option[Seq[Char]] = input match {
    case Nil => Some(result)
    case head +: tail => buildDict(head, dict) match {
      case None => None
      case newDict => runUncompress(tail, result ++ newDict.get.last, newDict.get)
    }
  }

  /** @inheritdoc */
  def uncompress(res: Seq[(Int, Char)]): Option[Seq[Char]] = runUncompress(res, Nil, empty)
}
