package simpol

import org.specs.runner.JUnit4
import org.specs.Specification
import Polynomial._
import Term._

class TermSpecTest extends JUnit4(TermSpec)

object TermSpec extends Specification {
  "term multiplication" should {
    "calculate 3*x*y^2 * 5*x*y^2 correctly" in {
      Term(3, 'x -> 1, 'y -> 2) * Term(5, 'x -> 1, 'y -> 2) mustEqual Term(15, 'x -> 2, 'y -> 4)
    }
    "calculate 3*x*y^2 * 9*x^2*z^3 correctly" in {
      Term(3, 'x -> 1, 'y -> 2) * Term(9, 'x -> 2, 'z -> 3) mustEqual Term(27, 'x -> 3, 'y -> 2, 'z -> 3)
    }
  }

  "term addition" should {
    "calculate 3*x*y^2 + 5*x*y^2" in {
      Term(3, 'x -> 1, 'y -> 2) + Term(5, 'x -> 1, 'y -> 2) mustEqual Term(8, 'x -> 1, 'y -> 2)
    }
  }

  "term simplification" should {
    "simplify zero" in {
      Term(0, 'x -> 1).simplify mustEqual Term(0)
    }
    "simplify zero exponent" in {
      Term(1, 'x -> 0, 'y -> 1).simplify mustEqual Term(1, 'y -> 1)
    }
  }
}