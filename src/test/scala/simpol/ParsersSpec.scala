package simpol

import org.specs.runner.JUnit4
import org.specs.Specification

class ParsersSpecTest extends JUnit4(ParsersSpec)

object ParsersSpec extends Specification with Parsers {
  "sum parser" should {
    "parse this sum correctly" in {
      parseAll(sum, "3*x*y") match {
        case Success(out, int) => out.toString mustEqual "3*(x^1)*(y^1)"
        case ns: NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "3*x*y+5*z*q") match {
        case Success(out, int) => out.toString mustEqual "3*(x^1)*(y^1)+5*(z^1)*(q^1)"
        case ns: NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "x-y") match {
        case Success(out, int) => out.toString mustEqual "1*(x^1)+-1*(y^1)"
        case ns: NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this sum correctly" in {
      parseAll(sum, "1-x+6") match {
        case Success(out, int) => out.toString mustEqual "7+-1*(x^1)"
        case ns: NoSuccess     => assert(false, ns.toString)
      }
    }
  }

  "term parser" should {
    "parse this term correctly" in {
      parseAll(term, "3*x*y") match {
        case Success(out, int) => out.toString mustEqual "3*(x^1)*(y^1)"
        case ns: NoSuccess     => assert(false, ns.toString)
      }
    }
    "parse this term correctly" in {
      parseAll(term, "3*(x^0)*(y^2)") match {
        case Success(out, int) => out.toString mustEqual "3*(x^0)*(y^2)"
        case ns: NoSuccess     => assert(false, ns.toString)
      }
    }
  }
}