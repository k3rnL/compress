package statistic

import compress.statistic._
import compress.statistic.BitConverter._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class ShannonFanoSuite extends AnyFunSuite with Matchers {

  test("ShannonFano algorithm compression and deflation on normal compression and deflation") {
    val shannonFano = new ShannonFano[Char]("hello world")
    val compressed = shannonFano.compress("hello world")
    compressed must be(Seq(Zero, Zero, One, Zero, Zero, Zero, Zero, One, One, One, One, One, Zero, Zero, Zero, Zero, One, Zero, One, Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, Zero, One, One))
    shannonFano.uncompress(compressed) must be(Some("hello world".toSeq))

    shannonFano.tree.get.meanLength must be (2.909090909090909)
    shannonFano.entropy must be (2.845350936622437)
  }
  
  test("ShannonFano algorithm compression and deflation empty source") {
    val nilShannonFano = new ShannonFano[Char]("")
    nilShannonFano.compress(Seq()) must be(Nil)
    nilShannonFano.uncompress(Seq()) must be(None)
  }

  test("ShannonFano algorithm tree and impossible deflation") {
    val simpleTree = new ShannonFano[Char]("aabbc")
    simpleTree.tree must be(Some(EncodingNode(5, EncodingNode(3, EncodingLeaf(1, 'c'), EncodingLeaf(2, 'b')), EncodingLeaf(2, 'a'))))
    simpleTree.uncompress(Seq(0)) must be(None)
  }

}
