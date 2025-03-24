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

    val rebalancedRoot: Rope = root.rebuild

    rebalancedRoot.isHeightBalanced shouldBe true
    rebalancedRoot.isWeightBalanced shouldBe true
  }

  trait RopeSpecScope {
    implicit val balance: Balance = Balance(3, 1)
  }

}
