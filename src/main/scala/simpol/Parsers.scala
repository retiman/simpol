package simpol

import scala.util.parsing.combinator.RegexParsers
import Polynomial._
import Term._

trait Parsers extends RegexParsers {
  def sum: Parser[Polynomial] = (
      term ~ rep(("+" | "-") ~ term) ^^ sumAction
    | term
  )

  def term: Parser[Polynomial] = (
      unary ~ rep("*" ~ term)        ^^ { case p ~ rest => rest.map(_ match { case "*" ~ t => t }).foldLeft(p)(_ * _) }
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

  def sumAction(result: ~[Polynomial, List[~[String, Polynomial]]]) = result match {
    case p ~ rest => rest.map(_ match { case "+" ~ t => t; case "-" ~ t => t * -1 }).foldLeft(p)(_ + _)
  }
}