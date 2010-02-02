Simpol
======

Simpol is a very simple polynomial expression simplifier written in Scala (meaning it takes a polynomial and represents it as a sum of products).  Powers will be expanded, so it may be the case that the "simplified" expression is more complicated.

Building
--------
Simpol uses SBT (http://code.google.com/p/simple-build-tool/) to build.  Download a copy or just use the SBT launcher bundled in the checkout.  These commands will package Simpol:

    bin/sbt
    update
    package

Usage
-----
After building, run this command, replacing EXPRESSION with a polynomial (in quotes):

    bin/simpol EXPRESSION

Restrictions
------------
- No spaces are allowed in the expression.
- Terms must be delimited by a `*`.
  e.g. 3*x*y is a valid expression, but 3xy is not.
- Sums must be delimited by a `+`, obviously.
- Coefficients to products must appear first in the term.
  e.g. 3*x*y is a valid expression, but x*y*3 is not.
- Parentheses are required when raising a sum to a power.
  e.g. (x+y+z)^2
- The binary subtraction operator is `-`; the unary negation operator is `_`.
  e.g. x-(_y) is a valid expression, but x-(-y) is not.

Grammar
-------
Using the Scala parser combinators makes parsing super easy.  Im too lazy to produce a grammar for valid polynomial expressions, but heres the parser class, which looks almost like BNF:

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