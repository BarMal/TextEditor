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

  it should "search" in new ChunkedRopeSpecScope {
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

  it should "handle large strings" in new RopeSpecScope {
    Rope(Range.inclusive(0, 50000000).map(_ => 'a').mkString).rebalance.rebuild
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
