package app.buffer

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
    a.index(0) shouldBe Some('H')
    a.index(10) shouldBe Some('n')
    a.index(12) shouldBe Some('m')
    a.index(23) shouldBe Some('y')
    a.index(24) shouldBe None
  }

  it should "split" in new RopeSpecScope {
    val a: Rope = Rope("Hello, my name is Barney")

    val Some(left, right) = a.split(12) : @unchecked
    left.collect() shouldBe "Hello, my na"
    right.collect() shouldBe "me is Barney"

    val Some(nothing, all) = a.split(0): @unchecked
    nothing.collect() shouldBe ""
    all.collect() shouldBe a.collect()

    val Some(all1, nothing1) = a.split(a.weight) : @unchecked
    all1.collect() shouldBe a.collect()
    nothing1.collect() shouldBe ""
  }

  it should "insert" in new RopeSpecScope {
    Rope("Hello world!").insert(5, ',').collect() shouldBe "Hello, world!"
  }

  it should "delete" in new RopeSpecScope {
    Rope("Hello, world!").delete(3, 2).collect() shouldBe "Hel, world!"
    Rope("Hello, world!").delete(13, 1).collect() shouldBe "Hello, world!"
    Rope("Hello, world!").delete(13, -14).collect() shouldBe ""
  }

  it should "replace" in new RopeSpecScope {
    Rope("Hello, world!").replace(5, '!').replace(7, 'W').collect() shouldBe "Hello! World!"
  }

  it should "handle large strings" in new RopeSpecScope {
    Rope(Range.inclusive(0, 50000000).map(_ => 'a').mkString).rebalance.rebuild
  }

  trait RopeSpecScope {
    given balance: Balance = Balance(3, 1, 5)
  }

}
