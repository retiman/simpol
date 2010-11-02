DESCRIPTION
===========
Simpol is a very simple polynomial expression "simplifier" written in Scala
(and by "simplify" I mean it takes a polynomial and represents it as a sum
of products).  This library is not intended for use in any kind of production
quality software; it is here only to serve as an example.

Powers will be expanded, so it may be the case that the "simplified" expression
is more complicated.

BUILDING
========
Use SBT to build; get it from the website here:

    http://code.google.com/p/simple-build-tool/

Then, create a script that launches sbt.  Here's an example:

    http://github.com/retiman/homekeeper/blob/master/bin/sbt

USAGE
=====
After building, run this command in SBT, replacing EXPRESSION with a polynomial
(in quotes):

    run "EXPRESSION"

EXAMPLES
--------

    run "(x+y+z)^4"
    Simplifying (x+y+z)^4:
      1*(y^4)+3*(y^2)*(x^2)+9*(y^2)*(z^1)*(x^1)+3*(z^3)*(x^1)+4*(z^3)*(y^1)+7*(z^2)*(y^1)*(x^1)+4*(z^2)*(y^2)+8*(z^1)*(x^2)*(y^1)+4*(z^1)*(y^3)+1*(z^4)+5*(x^2)*(z^2)+3*(x^3)*(z^1)+3*(x^1)*(y^3)+3*(x^3)*(y^1)+1*(x^4)


RESTRICTIONS
------------
- No spaces are allowed in the expression.
- Terms must be delimited by a `*`.
  e.g. `3*x*y` is a valid expression, but `3xy` is not.
- Sums must be delimited by a `+`, obviously.
- Coefficients to products must appear first in the term.
  e.g. `3*x*y` is a valid expression, but `x*y*3` is not.
- Parentheses are required when raising a sum to a power.
  e.g. `(x+y+z)^2`
- The binary subtraction operator is `-`; the unary negation operator is `_`.
  e.g. `x-(_y)` is a valid expression, but `x-(-y)` is not.

GRAMMAR
=======
Using the Scala parser combinators makes parsing super easy.  Im too lazy to
produce a grammar for valid polynomial expressions, but heres the parser class,
which looks almost like BNF:

    trait Parsers extends RegexParsers {
      def sum: Parser[Polynomial] = (
          term ~ rep(("+" | "-") ~ sum)
        | term
      )

      def term: Parser[Polynomial] = (
          unary ~ rep("*" ~ term)
        | unary
      )

      def unary: Parser[Polynomial] = (
          "_" ~> primary
        | primary
      )

      def primary: Parser[Polynomial] = (
          variable ~ "^" ~ constant
        | variable
        | constant
        | "(" ~> sum ~ ")^" ~ constant
        | "(" ~> sum <~ ")"
      )

      def variable = """[a-z]""".r
      def constant = """\d+""".r
    }

You'll be able to figure it out.
