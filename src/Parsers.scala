package simpol

import scala.util.parsing.combinator.RegexParsers
import Polynomial._

trait Parsers extends RegexParsers {
  def sum: Parser[Polynomial] = (
      term ~ rep(("+" | "-") ~ term) ^^ { case p ~ rest => rest.map(sumAction _).foldLeft(p)(_ + _) }
    | term
  )

  def term: Parser[Polynomial] = (
      unary ~ rep("*" ~ term)        ^^ { case p ~ rest => rest.map(_._2).foldLeft(p)(_ * _) }
    | unary
  )

  def unary: Parser[Polynomial] = (
      "_" ~> primary                 ^^ { case p => p * -1 }
    | primary
  )

  def primary: Parser[Polynomial] = (
      variable ~ "^" ~ constant      ^^ { case v ~ "^" ~ e  => Polynomial(v -> e) }
    | variable                       ^^ { case v => Polynomial(v -> 1) }
    | constant                       ^^ { case c => Polynomial(c) }
    | "(" ~> sum ~ ")^" ~ constant   ^^ { case s ~ ")^" ~ c => List.make(c, s).reduceLeft(_ * _) }
    | "(" ~> sum <~ ")"
  )

  def variable = """[a-z]""".r       ^^ { case s => Symbol(s) }
  def constant = """\d+""".r         ^^ { case c => c.toInt }

  def sumAction(result: Any): Polynomial = {
    def coerce(result: Any) = result match { case p: Polynomial => p }
    result match {
      case "+" ~ p => coerce(p)
      case "-" ~ p => coerce(p) * -1
    }
  }
}
