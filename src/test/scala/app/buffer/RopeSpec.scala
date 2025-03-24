package app.buffer

import app.buffer.Rope.{Leaf, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeSpec extends AnyFlatSpec with Matchers {

  "Rope" should "collect values and weights correctly" in {
    val c = Node(Some(Leaf("Hello ")), Some(Leaf("my ")))
    c.weight shouldBe 6
    val g = Node(Some(Leaf("na")), Some(Leaf("me i")))
    g.weight shouldBe 2
    val h = Node(Some(Leaf("s")), Some(Leaf(" Barney")))
    h.weight shouldBe 1

    val d = Node(Some(g), Some(h))
    d.weight shouldBe 6
    val b = Node(Some(c), Some(d))
    b.weight shouldBe 9

    val a = Node(Some(b), None)
    a.weight shouldBe 23

    a.collect shouldBe "Hello my name is Barney"
  }

}
