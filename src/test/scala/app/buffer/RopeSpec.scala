package app.buffer

import app.buffer.rope.{Balance, Leaf, Node, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeSpec extends AnyFlatSpec with Matchers {

  "Rope" should "weight" in new RopeSpecScope {
    val c: Node = Node(Leaf("Hello "), Leaf("my "))
    c.weight shouldBe 9
    val g: Node = Node(Leaf("na"), Leaf("me i"))
    g.weight shouldBe 6
    val h: Node = Node(Leaf("s"), Leaf(" Barney"))
    h.weight shouldBe 8

    val d: Node = Node(g, h)
    d.weight shouldBe 14
    val b: Node = Node(c, d)
    b.weight shouldBe 23

    val a: Node = Node(b, Leaf(""))
    a.weight shouldBe 23

    a.collect() shouldBe "Hello my name is Barney"
  }

  it should "concat" in new RopeSpecScope {
    (Rope(" world!") :: Rope("Hello")).collect() shouldBe "Hello world!"
  }

  it should "balance" in new RopeSpecScope {
    val depth4: Node  = Node(Leaf("Deepest"), Leaf(""))
    val depth3a: Node = Node(depth4, Node(Leaf("Up1"), Leaf("")))
    val depth3b: Node = Node(Leaf("Up1a"), Leaf("Up1b"))
    val depth2a: Node = Node(depth3a, depth3b)
    val depth2b: Node = Node(Leaf("Up2"), Leaf(""))
    val root: Node    = Node(depth2a, depth2b)

    root.isHeightBalanced shouldBe false
    root.isWeightBalanced shouldBe false

    val rebuiltRoot: Rope = root.rebuild
    rebuiltRoot.isHeightBalanced shouldBe true
    rebuiltRoot.isWeightBalanced shouldBe true

    val rebalancedRoot: Rope = root.rebalance
    rebalancedRoot.isHeightBalanced shouldBe true
    rebalancedRoot.isWeightBalanced shouldBe true
  }

  it should "index" in new RopeSpecScope {
    val a: Rope = Rope("Hello, my name is Barney")

    a.collect().zipWithIndex.map { case (char, index) =>
      a.index(index) shouldBe Some(char)
    }

    a.index(-1) shouldBe None
    a.index(a.weight + 1) shouldBe None
  }

  it should "split" in new RopeSpecScope {
    val a: Rope = Rope("Hello, my name is Barney")

    List(0, 12, a.weight + 1).foreach { scenarioIndex =>
      a.splitAt(scenarioIndex).map { (l, r) =>
        val (el, er) = a.collect().splitAt(scenarioIndex)
        l.collect() shouldBe el
        r.collect() shouldBe er
      }
    }
  }

  it should "insert" in new RopeSpecScope {
    Rope("Hello world!").insert(5, ',').collect() shouldBe "Hello, world!"
  }

  it should "delete" in new RopeSpecScope {
    Rope("Hello, world!").deleteLeft(3, 2).collect() shouldBe "Ho, world!"
    Rope("Hello, world!").deleteLeft(3, 12).collect() shouldBe "lo, world!"
    Rope("Hello, world!").deleteRight(13, 1).collect() shouldBe "Hello, world!"
    Rope("Hello, world!").deleteRight(13, -13).collect() shouldBe ""
    Rope("Hello, world!").deleteLeft(0, -13).collect() shouldBe ""
  }

  it should "replace" in new RopeSpecScope {
    Rope("Hello, world!")
      .replace(5, '!')
      .replace(7, 'W')
      .collect() shouldBe "Hello! World!"
  }

  it should "search for first occurrence" in new ChunkedRopeSpecScope {
    val lorem0: String =
      s"""Lorem ipsum dolor sit amet, consectetur adipiscing
         |elit, sed do eiusmod tempor incididunt ut labore et
         |dolore magna aliqua. Ut enim ad minim veniam, quis
         |nostrud exercitation ullamco laboris nisi ut aliquip
         |ex ea commodo consequat. Duis aute irure dolor in
         |reprehenderit in voluptate velit esse cillum dolore
         |eu fugiat nulla pariatur. Excepteur sint occaecat
         |cupidatat non proident, sunt in culpa qui officia
         |deserunt mollit anim id est laborum.""".stripMargin

    val rope: Rope = Rope(lorem0)

    val search0: String = "Lorem"
    rope.search(search0) shouldBe Some(0)

    val search1: String = "laborum"
    rope.search(search1) shouldBe Some(lorem0.indexOf(search1))

    val search2: String = "in culpa qui officia"
    rope.search(search2) shouldBe Some(lorem0.indexOf(search2))

    val search3: String = "Doesn't exist in the body"
    rope.search(search3) shouldBe None
  }

  it should "search for all occurrences" in new ChunkedRopeSpecScope {
    val text = "the cat sat on the mat with the rat"
    val rope = Rope(text)

    val results = rope.searchAll("the")
    val expected = List(0, 15, 28) // All positions of "the"

    results should contain theSameElementsAs expected
  }

  it should "find overlapping matches" in new ChunkedRopeSpecScope {
    val text = "aaaaaa"
    val rope = Rope(text)

    val results = rope.searchAll("aa")
    // Should find: positions 0, 1, 2, 3, 4
    results.length should be >= 5
  }

  it should "search across leaf boundaries" in new ChunkedRopeSpecScope {
    // With leafChunkSize = 30, create a pattern that spans boundaries
    val text = "a" * 28 + "boundary" + "b" * 28
    val rope = Rope(text)

    rope.search("boundary") shouldBe Some(28)
    rope.searchAll("boundary") should contain(28)
  }

  it should "handle repeated patterns" in new ChunkedRopeSpecScope {
    val text = "abc abc abc abc"
    val rope = Rope(text)

    val results = rope.searchAll("abc")
    results should contain theSameElementsAs List(0, 4, 8, 12)
  }

  it should "handle patterns at boundaries" in new ChunkedRopeSpecScope {
    // Create text where pattern spans leaf boundary
    val part1 = "x" * 28 + "te"
    val part2 = "st" + "y" * 28
    val text = part1 + part2
    val rope = Rope(text)

    rope.search("test") shouldBe Some(28)
  }

  it should "handle single character searches" in new ChunkedRopeSpecScope {
    val text = "abcabc"
    val rope = Rope(text)

    val results = rope.searchAll("a")
    results should contain theSameElementsAs List(0, 3)
  }

  it should "handle search at end of rope" in new ChunkedRopeSpecScope {
    val text = "hello world test"
    val rope = Rope(text)

    rope.search("test") shouldBe Some(12)
    rope.searchAll("test") should contain(12)
  }

  it should "handle empty search results" in new ChunkedRopeSpecScope {
    val text = "hello world"
    val rope = Rope(text)

    rope.search("xyz") shouldBe None
    rope.searchAll("xyz") shouldBe empty
  }

  it should "handle case-sensitive searches" in new ChunkedRopeSpecScope {
    val text = "Hello hello HELLO"
    val rope = Rope(text)

    rope.searchAll("hello") should contain theSameElementsAs List(6)
    rope.searchAll("Hello") should contain theSameElementsAs List(0)
    rope.searchAll("HELLO") should contain theSameElementsAs List(12)
  }

  it should "handle multiline searches" in new ChunkedRopeSpecScope {
    val text = "line1\npattern\nline3\npattern\nline5"
    val rope = Rope(text)

    val results = rope.searchAll("pattern")
    results.length shouldBe 2
    results should contain theSameElementsAs List(6, 20)
  }

  it should "handle long patterns" in new ChunkedRopeSpecScope {
    val pattern = "this is a very long pattern that spans multiple chunks"
    val text = "prefix " + pattern + " middle " + pattern + " suffix"
    val rope = Rope(text)

    val results = rope.searchAll(pattern)
    results.length shouldBe 2
  }

  it should "handle patterns with special characters" in new ChunkedRopeSpecScope {
    val text = "test\nwith\ttabs and spaces"
    val rope = Rope(text)

    rope.search("\n") shouldBe Some(4)
    rope.search("\t") shouldBe Some(9)
    rope.search(" ") shouldBe Some(14)
  }

  it should "replaceAll correctly" in new ChunkedRopeSpecScope {
    val text = "the cat sat on the mat"
    val rope = Rope(text)

    val replaced = rope.replaceAll("the", "a")
    replaced.collect() shouldBe "a cat sat on a mat"
  }

  it should "replaceAll with longer replacement" in new ChunkedRopeSpecScope {
    val text = "a b a b a"
    val rope = Rope(text)

    val replaced = rope.replaceAll("a", "long")
    replaced.collect() shouldBe "long b long b long"
  }

  it should "handle large strings" in new RopeSpecScope {
    val largeRope = Rope(Range.inclusive(0, 50000000).map(_ => 'a').mkString)
    largeRope.rebalance.rebuild
  }

  trait RopeSpecScope {
    given balance: Balance =
      Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 5)
  }

  trait ChunkedRopeSpecScope {
    given balance: Balance =
      Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)
  }
}