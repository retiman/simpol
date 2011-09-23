package simpol

import org.specs.SpecificationWithJUnit
import Polynomial._

class ParsersSpec extends SpecificationWithJUnit {
  val p = new Parsers {}
  import p._

  "sum parser" should {
    "parse this sum correctly" in {
      parseAll(sum, "3*x*y") match {
        case Success(result, input) => result.toString mustEqual "3*(x^1)*(y^1)"
        case Failure(msg, input)    => assert(false, msg)
        case _                      => assert(false)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "3*x*y+5*z*q") match {
        case Success(result, input) => result.toString mustEqual "3*(x^1)*(y^1)+5*(z^1)*(q^1)"
        case Failure(msg, input)    => assert(false, msg)
        case _                      => assert(false)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "x-y") match {
        case Success(result, input) => result.toString mustEqual "1*(x^1)+-1*(y^1)"
        case Failure(msg, input)    => assert(false, msg)
        case _                      => assert(false)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "1-x+6") match {
        case Success(result, input) => result.toString mustEqual "7+-1*(x^1)"
        case Failure(msg, input)    => assert(false, msg)
        case _                      => assert(false)
      }
    }
  }

  "term parser" should {
    "parse this term correctly" in {
      parseAll(term, "3*x*y") match {
        case Success(result, input) => result.toString mustEqual "3*(x^1)*(y^1)"
        case Failure(msg, input)    => assert(false, msg)
        case _                      => assert(false)
      }
    }
    "parse this term correctly" in {
      parseAll(term, "3*(x^0)*(y^2)") match {
        case Success(result, input) => result.toString mustEqual "3*(x^0)*(y^2)"
        case Failure(msg, input)    => assert(false, msg)
        case _                      => assert(false)
      }
    }
  }
}
