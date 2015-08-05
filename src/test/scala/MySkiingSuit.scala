/**
 * Created by crankycode on 8/5/15.
 */

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

class MySkiingSuit extends FunSuite {
  val graph2 = Vector(
    Vector(4,8,7,3),
    Vector(2,5,9,3),
    Vector(6,3,2,5),
    Vector(4,4,1,6))

  test("Testing recursion") {
    MySkiing.travers(0,1,graph2, 0, Seq())
  }

}
