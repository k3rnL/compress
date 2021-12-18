import compress.RLE
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class RLESuite extends AnyFunSuite with Matchers
  {

    test("RLE algorithm") {
      val rle: RLE[Char] = new RLE()
      rle.compress("aaaabbb") must be (Seq(('a', 4), ('b', 3)))
      rle.uncompress(Seq(('a', 4), ('b', 3))) must be (Some("aaaabbb".toSeq))

      rle.compress(Seq()) must be (Nil)
      rle.uncompress(Seq()) must be (None)

      rle.uncompress(Seq(('q', -4))) must be (None)

    }

  }
