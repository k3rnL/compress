package lz

import compress.lz.LZ78
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class LZ78Suite extends AnyFunSuite with Matchers {

  test("LZ78 algorithm compression and deflation on normal compression and deflation") {
    val compressed = LZ78.compress("belle echelle !")
    compressed must be(Seq((0, 'b'), (0, 'e'), (0, 'l'), (3, 'e'), (0, ' '), (2, 'c'), (0, 'h'), (2, 'l'), (4, ' '), (0, '!')))

    LZ78.uncompress(compressed) must be(Some("belle echelle !".toSeq))
  }

  test("LZ78 algorithm compression and deflation empty source") {
    LZ78.compress(Seq()) must be(Seq())
    LZ78.uncompress(Seq()) must be(Some(Seq()))
  }

  test("LZ78 algorithm impossible deflation") {
    val compressedWithErrors = Seq((0, 'b'), (23, 'e'), (-4, 'l'), (3, 'e'))
    LZ78.uncompress(compressedWithErrors) must be(None)
  }
}
