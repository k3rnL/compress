package lz

import compress.lz.LZW
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class LZWSuite extends AnyFunSuite with Matchers {

  test("LZW algorithm compression and deflation on normal compression and deflation") {
    val LZW = new LZW()

    val compressed = LZW.compress("belle echelle")
    compressed must be(Seq(98, 101, 108, 108, 101, 32, 101, 99, 104, 257, 259))

    LZW.uncompress(compressed) must be(Some("belle echelle".toSeq))
  }

  test("LZW algorithm compression and deflation empty source") {
    val LZW = new LZW()

    LZW.compress("") must be(Seq())
    LZW.uncompress(Seq()) must be(Some(Seq()))
  }

  test("LZW algorithm impossible deflation") {
    val LZW = new LZW()
    val compressedWithErrors = Seq(98, 300, 32)
    LZW.uncompress(compressedWithErrors) must be(None)
  }
}
