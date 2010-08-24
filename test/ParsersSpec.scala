package simpol

import org.specs.Specification
import Polynomial._

// TODO: ParsersSpec cannot have trait Parsers because Specification already
// defines Error (which conflicts with RegexParsers.Error).  Figure out how to
// fix this.
object ParsersSpec extends Specification {
  // This little hack gets around the problem.
  val parsers = new Parsers {}
  import parsers._

  "sum parser" should {
    "parse this sum correctly" in {
      parsers.parseAll(parsers.sum, "3*x*y") match {
        case parsers.Success(out, int) => out.toString mustEqual "3*(x^1)*(y^1)"
        case ns: parsers.NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this sum correctly" in {
      parsers.parseAll(parsers.sum, "3*x*y+5*z*q") match {
        case parsers.Success(out, int) => out.toString mustEqual "3*(x^1)*(y^1)+5*(z^1)*(q^1)"
        case ns: parsers.NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this sum correctly" in {
      parsers.parseAll(parsers.sum, "x-y") match {
        case parsers.Success(out, int) => out.toString mustEqual "1*(x^1)+-1*(y^1)"
        case ns: parsers.NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this sum correctly" in {
      parsers.parseAll(parsers.sum, "1-x+6") match {
        case parsers.Success(out, int) => out.toString mustEqual "7+-1*(x^1)"
        case ns: parsers.NoSuccess     => assert(false, ns.toString)
      }
    }
  }

  "term parser" should {
    "parse this term correctly" in {
      parsers.parseAll(parsers.term, "3*x*y") match {
        case parsers.Success(out, int) => out.toString mustEqual "3*(x^1)*(y^1)"
        case ns: parsers.NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this term correctly" in {
      parsers.parseAll(parsers.term, "3*(x^0)*(y^2)") match {
        case parsers.Success(out, int) => out.toString mustEqual "3*(x^0)*(y^2)"
        case ns: parsers.NoSuccess     => assert(false, ns.toString)
      }
    }
  }
}
