package simpol

import scala.util.parsing.combinator.RegexParsers
import Polynomial._

trait Parsers extends RegexParsers {
  def sum: Parser[Polynomial] = (
      term ~ rep("+" ~ sum)      ^^ { case p ~ rest => rest.map(_ match { case "+" ~ t => t }).foldLeft(p)(_ + _) }
    | term ~ rep("-" ~ sum)      ^^ { case p ~ rest => rest.map(_ match { case "-" ~ t => t }).foldLeft(p)(_ - _) }
    | term
  )

  def term: Parser[Polynomial] = (
      unary ~ rep("*" ~ term)    ^^ { case p ~ rest => rest.map(_ match { case "*" ~ t => t }).foldLeft(p)(_ * _) }
    | unary
  )

  def unary: Parser[Polynomial] = (
      "_" ~> primary             ^^ { case p => NEG_ONE * p }
    | primary
  )

  def primary: Parser[Polynomial] = (
      variable ~ "^" ~ constant  ^^ { case v ~ "^" ~ e => Polynomial(Set(Term(1, Map(v -> e)))) }
    | variable                   ^^ { case v => Polynomial(Set(Term(1, Map(v -> 1)))) }
    | constant                   ^^ { case c => Polynomial(Set(Term(c, Map()))) }
    | "(" ~> sum <~ ")"          ^^ { case s => s }
  )

  def variable = """[a-z]""".r   ^^ { Symbol(_) }
  def constant = """\d+""".r     ^^ { _.toInt }
}