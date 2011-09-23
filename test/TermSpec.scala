package simpol

import org.specs.SpecificationWithJUnit
import Term._

class TermSpec extends SpecificationWithJUnit {
  "term multiplication" should {
    "calculate 3*x*y^2 * 5*x*y^2 correctly" in {
      val expected = Term(3, 'x -> 1, 'y -> 2) * Term(5, 'x -> 1, 'y -> 2)
      val result = Term(15, 'x -> 2, 'y -> 4)
      expected mustEqual result
    }
    "calculate 3*x*y^2 * 9*x^2*z^3 correctly" in {
      val expected = Term(3, 'x -> 1, 'y -> 2) * Term(9, 'x -> 2, 'z -> 3)
      val result = Term(27, 'x -> 3, 'y -> 2, 'z -> 3)
      expected mustEqual result
    }
  }

  "term addition" should {
    "calculate 3*x*y^2 + 5*x*y^2" in {
      val expected = Term(3, 'x -> 1, 'y -> 2) + Term(5, 'x -> 1, 'y -> 2)
      val result = Term(8, 'x -> 1, 'y -> 2)
      expected mustEqual result
    }
  }

  "term simplification" should {
    "simplify zero" in {
      val expected = Term(0, 'x -> 1).simplify
      val result = Term(0)
      expected mustEqual result
    }
    "simplify zero exponent" in {
      val expected = Term(1, 'x -> 0, 'y -> 1).simplify
      val result = Term(1, 'y -> 1)
      expected mustEqual result
    }
  }
}
