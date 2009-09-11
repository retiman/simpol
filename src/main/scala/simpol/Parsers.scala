package simpol

import scala.util.parsing.combinator.RegexParsers
import Polynomial._
import Term._

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
      "_" ~> primary                 ^^ { _ * -1 }
    | primary
  )

  def primary: Parser[Polynomial] = (
      variable ~ "^" ~ constant      ^^ { case v ~ "^" ~ e => Polynomial(v -> e) }
    | variable                       ^^ { case v => Polynomial(v -> 1) }
    | constant                       ^^ { Polynomial(_) }
    | "(" ~> sum ~ ")^" ~ constant   ^^ { case s ~ ")^" ~ c => List.make(c, s).reduceLeft(_ * _) }
    | "(" ~> sum <~ ")"
  )

  def variable = """[a-z]""".r       ^^ { Symbol(_) }
  def constant = """\d+""".r         ^^ { _.toInt }

  def sumAction(result: ~[String, Polynomial]) = result match {
    case "+" ~ p => p
    case "-" ~ p => p * -1
  }
}