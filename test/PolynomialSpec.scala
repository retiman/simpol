package simpol

import org.specs.Specification
import Polynomial._

object PolynomialSpec extends Specification {
  "polynomial multiplication" should {
    "multiply zero polynomials correctly" in { 
      Polynomial(0) * Polynomial(1) mustEqual Polynomial(0)
      Polynomial(1) * Polynomial(0) mustEqual Polynomial(0)
    }
    "multiply out correctly (first, inside, outside, last)" in {
      val t1 = Term(3, 'x -> 1, 'y -> 2)
      val t2 = Term(9, 'x -> 2, 'z -> 3)
      val t3 = Term(5, 'x -> 1, 'y -> 2)
      val t4 = Term(1, 'x -> 1, 'z -> 3)
      val p1 = Polynomial(t1, t2)
      val p2 = Polynomial(t3, t4)
      p1 * p2 mustEqual Polynomial(t1 * t3, t2 * t3, t1 * t4, t2 * t4)
    }
    "multiply by -1 correctly" in {
      val p1 = Polynomial(-1)
      val p2 = Polynomial(Term(3, 'x -> 1, 'y -> 2),  Term(5, 'x -> 1, 'y -> 2))
      val p3 = Polynomial(Term(-3, 'x -> 1, 'y -> 2), Term(-5, 'x -> 1, 'y -> 2))
      p1 * p2 mustEqual p3
    }
  }

  "polynomial addition" should {
    "add like terms correctly" in {
      val p1 = Polynomial(Term(3, 'x -> 1, 'y -> 1), Term(1, 'x -> 2, 'y -> 2))
      val p2 = Polynomial(Term(5, 'x -> 1, 'y -> 1), Term(7, 'x -> 2, 'y -> 2))
      val p3 = Polynomial(Term(8, 'x -> 1, 'y -> 1), Term(8, 'x -> 2, 'y -> 2))
      p1 + p2 mustEqual p3
    }
    "add unlike terms correctly" in {
      val t1 = Term(3, 'a -> 1, 'b -> 1)
      val t2 = Term(1, 'c -> 2, 'd -> 2)
      val t3 = Term(5, 'e -> 1, 'f -> 1)
      val t4 = Term(7, 'g -> 2, 'h -> 2)
      val p1 = Polynomial(t1, t2)
      val p2 = Polynomial(t3, t4)
      val p3 = Polynomial(t1, t2, t3, t4)
      p1 + p2 mustEqual p3
    }
  }

  "polynomial simplification" should {
    "simplifying addition by zero" in {
      val px = Polynomial(Term(1, 'x -> 1, 'y -> 2, 'z -> 3))
      (Polynomial(0) + px).simplify mustEqual px
      (px + Polynomial(0)).simplify mustEqual px
    }
    "simplify zero terms" in {
      Polynomial(Term(0, 'x -> 1)).simplify mustEqual Polynomial(0)
    }
  }

  "polynomial printing" should {
    "display this polynomial correctly" in {
      Polynomial(Term(3, 'x -> 1, 'y -> 2), Term(9, 'x -> 2, 'z -> 3)).toString mustEqual "3*(x^1)*(y^2)+9*(x^2)*(z^3)"
    }
  }
}
