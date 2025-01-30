package ru.otus.module1.homework1

import org.scalatest.flatspec.AnyFlatSpec
import ru.otus.module1.list._
import ru.otus.module1.opt._

class fp_concepts_test extends AnyFlatSpec {
  "Lists methods" should "work correctly" in {
    val l1: List[Int] = List(1, 2, 3)
    val l2: List[Int] = List.apply(3, 2, 1)
    l1.mkString(",")
    l2.mkString("-")

    val l3 = l1.reverse()
    l3.mkString("+")

    val l4 = l3.map(x => x * 2)
    l4.mkString(" -> ")

    val l5 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter(x => x % 2 == 0)
    l5.mkString(" - ")

    val ffn: Int => Int = x => x + 1

    val l6 = l5.map(ffn)
    l6.mkString(" -- ")

    val l7 = incList(l6)
    l7.mkString(" --- ")

    val l8 = List("a", "b", "c")
    val l9 = shoutString(l8)
    l9.mkString(" ---- ")
  }

  "Option" should "work correctly" in {
    val o1 = Option(3)
    assert(!o1.isEmpty)

    val vO1 = o1.get
    assert(vO1 == 3)

    val mO1 = o1 map (x => x * 2)
    assert(mO1.get == 6)

    val o2 = Option("A")
    val zO1 = o1 zip o2
    assert(zO1.get == (3, "A"))

    o2.printIfAny()

    val o3 = o1.filter(x => x % 2 == 0)
    o3.printIfAny()
    val ex = intercept[Exception] {
      o3.get
    }

    assert(ex.getMessage == "get on empty option")


  }
}
