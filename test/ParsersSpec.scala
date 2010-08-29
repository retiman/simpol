package simpol

import org.specs.Specification
import Polynomial._

object ParsersSpec extends Specification {
  val p = new Parsers {}
  import p._

  "sum parser" should {
    "parse this sum correctly" in {
      parseAll(sum, "3*x*y") match {
        case Success(result, input) => result.toString mustEqual "3*(x^1)*(y^1)"
        case Failure(msg, input)    => assert(false, msg)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "3*x*y+5*z*q") match {
        case Success(result, input) => result.toString mustEqual "3*(x^1)*(y^1)+5*(z^1)*(q^1)"
        case Failure(msg, input)    => assert(false, msg)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "x-y") match {
        case Success(result, input) => result.toString mustEqual "1*(x^1)+-1*(y^1)"
        case Failure(msg, input)    => assert(false, msg)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "1-x+6") match {
        case Success(result, input) => result.toString mustEqual "7+-1*(x^1)"
        case Failure(msg, input)    => assert(false, msg)
      }
    }
  }

  "term parser" should {
    "parse this term correctly" in {
      parseAll(term, "3*x*y") match {
        case Success(result, input) => result.toString mustEqual "3*(x^1)*(y^1)"
        case Failure(msg, input)    => assert(false, msg)
      }
    }
    "parse this term correctly" in {
      parseAll(term, "3*(x^0)*(y^2)") match {
        case Success(result, input) => result.toString mustEqual "3*(x^0)*(y^2)"
        case Failure(msg, input)    => assert(false, msg)
      }
    }
  }
}
