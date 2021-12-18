package compress.statistic

import scala.language.implicitConversions

/** Implementation of a bit */
sealed trait Bit
case object Zero extends Bit
case object One  extends Bit

object BitConverter {
  implicit def intToBit(int: Int): Bit = int match {
    case 0 => Zero
    case any => One
  }

  implicit def bitToInt(bit: Bit): Int = bit match {
    case Zero => 0
    case One => 1
  }

}