package statistic

import compress.statistic._
import compress.statistic.BitConverter._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class HuffmanSuite extends AnyFunSuite with Matchers {

  test("Huffman algorithm compression and deflation on normal compression and deflation") {
    val huffman = new Huffman[Char]("hello world")
    val compressed = huffman.compress("hello world")
    compressed must be(Seq(Zero, Zero, Zero, One, One, One, Zero, One, Zero, One, Zero, One, One, Zero, One, One, One, One, Zero, One, Zero, One, One, Zero, Zero, Zero, One, One, Zero, Zero, One, One))
    huffman.uncompress(compressed) must be(Some("hello world".toSeq))

    huffman.tree.get.meanLength must be (2.909090909090909)
    huffman.entropy must be (2.845350936622437)
  }

  test("Huffman algorithm compression and deflation empty source") {
    val nilHuffman = new Huffman[Char]("")
    nilHuffman.compress(Seq()) must be(Nil)
    nilHuffman.uncompress(Seq()) must be(None)
  }

  test("Huffman algorithm tree and impossible deflation") {
    val simpleTree = new Huffman[Char]("aabbc")
    simpleTree.tree must be(Some(EncodingNode(5, EncodingLeaf(2, 'a'), EncodingNode(3, EncodingLeaf(1, 'c'), EncodingLeaf(2, 'b')))))
    simpleTree.uncompress(Seq(1)) must be(None)
  }

}
