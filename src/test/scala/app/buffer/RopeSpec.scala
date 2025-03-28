package app.buffer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeSpec extends AnyFlatSpec with Matchers {

  "Rope" should "collect values and weights correctly" in new RopeSpecScope {
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

  it should "concat another Rope" in new RopeSpecScope {
    (Rope(" world!") :: Rope("Hello")).collect() shouldBe "Hello world!"
  }

  it should "evaluate and correct balance" in new RopeSpecScope {
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
    val c: Node = Node(Leaf("Hello "), Leaf("my "))
    val g: Node = Node(Leaf("na"), Leaf("me i"))
    val h: Node = Node(Leaf("s"), Leaf(" Barney"))
    val d: Node = Node(g, h)
    val b: Node = Node(c, d)
    val a: Node = Node(b)

    a.collect().zipWithIndex.map {
      case (char, index) => a.indexOf(index) shouldBe Some(char)
    }

    a.indexOf(-1) shouldBe None
    a.indexOf(0) shouldBe Some('H')
    a.indexOf(10) shouldBe Some('a')
    a.indexOf(12) shouldBe Some('e')
    a.indexOf(22) shouldBe Some('y')
    a.indexOf(23) shouldBe None
  }

  trait RopeSpecScope {
    given balance: Balance = Balance(3, 1)
  }

}
