package simpol

import org.specs.runner.JUnit4
import org.specs.Specification
import Polynomial._
import Term._

class PolynomialSpecTest extends JUnit4(PolynomialSpec)

object PolynomialSpec extends Specification {
  val t1 = Term(3, Map('x -> 1, 'y -> 2))
  val t2 = Term(5, Map('x -> 1, 'y -> 2))
  val t3 = Term(9, Map('x -> 2, 'z -> 3))
  val t4 = Term(1, Map('x -> 1, 'z -> 3))
  val t5 = Term(8, Map('x -> 1, 'y -> 2))
  val t6 = Term(-1)
  val t7 = Term(-3, Map('x -> 1, 'y -> 2))
  val t8 = Term(-5, Map('x -> 1, 'y -> 2))
  val p1 = Polynomial(Set(t1, t3))
  val p2 = Polynomial(Set(t2, t4))

  "polynomials" should {
    "multiply correctly" in { Polynomial(0) * p1 mustEqual Polynomial(0) }
    "multiply correctly" in { p1 * p2 mustEqual Polynomial(Set(t1 * t2, t1 * t4, t3 * t2, t3 * t4)) }
    "multiply correctly" in { Polynomial(Set(t6)) * Polynomial(Set(t1, t2)) mustEqual Polynomial(Set(t7, t8)) }
    "add correctly" in { (Polynomial(0) + p1).simplify mustEqual p1 }
    "add correctly" in { p1 + p2 mustEqual Polynomial(Set(t5, t3, t4)) }
    "simplify correctly" in { Polynomial(Set(t1, t2)).simplify mustEqual Polynomial(Set(t5)) }
    "simplify correctly" in { Polynomial(0) mustEqual Polynomial(Set(Term(0, Map('x -> 1)))).simplify }
    "simplify correctly" in { (Polynomial(0) + Polynomial(0)).simplify mustEqual Polynomial(0).simplify }
    "print correctly" in { Polynomial(Set(t1, t3)).toString mustEqual "3*(x^1)*(y^2)+9*(x^2)*(z^3)" }
  }
}