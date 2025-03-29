package app.buffer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeSpec extends AnyFlatSpec with Matchers {

  "Rope" should "weight" in new RopeSpecScope {
    val c: Node = Node(Leaf("Hello "), Leaf("my "))
    c.weight shouldBe 6
    val g: Node = Node(Leaf("na"), Leaf("me i"))
    g.weight shouldBe 2
    val h: Node = Node(Leaf("s"), Leaf(" Barney"))
    h.weight shouldBe 1

    val d: Node = Node(g, h)
    d.weight shouldBe 6
    val b: Node = Node(c, d)
    b.weight shouldBe 9

    val a: Node = Node(b)
    a.weight shouldBe 23

    a.collect() shouldBe "Hello my name is Barney"
  }

  it should "concat" in new RopeSpecScope {
    (Rope(" world!") :: Rope("Hello")).collect() shouldBe "Hello world!"
  }

  it should "balance" in new RopeSpecScope {
    val depth4: Node  = Node(Leaf("Deepest"))
    val depth3a: Node = Node(depth4, Node(Leaf("Up1")))
    val depth3b: Node = Node(Leaf("Up1a"), Leaf("Up1b"))
    val depth2a: Node = Node(depth3a, depth3b)
    val depth2b: Node = Node(Leaf("Up2"))
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
    a.index(10) shouldBe Some('a')
    a.index(12) shouldBe Some('e')
    a.index(22) shouldBe Some('y')
    a.index(23) shouldBe None
  }

  it should "split" in new RopeSpecScope {
    val a: Rope = Rope("Hello, my name is Barney")

    val (left, right) = a.split(12)
    left.map(_.collect()) shouldBe Some("Hello, my na")
    right.map(_.collect()) shouldBe Some("me is Barney")

    val (nothing, all) = a.split(0)
    nothing.map(_.collect()) shouldBe None
    all.map(_.collect()) shouldBe Some(a.collect())

    val (all1, nothing1) = a.split(a.collect().length - 1)
    nothing1.map(_.collect()) shouldBe Some("")
    all1.map(_.collect()) shouldBe Some(a.collect())
  }

  it should "insert" in new RopeSpecScope {
    Rope("Hello world!").insert(5, ',').collect() shouldBe "Hello, world!"
  }

  it should "delete" in new RopeSpecScope {}

  trait RopeSpecScope {
    given balance: Balance = Balance(3, 1, 5)
  }

}
