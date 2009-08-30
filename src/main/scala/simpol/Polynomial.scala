package simpol

import Polynomial._

object Polynomial {
  type Variable = Symbol
  type Exponent = Int
  val ONE = Polynomial(Set(Term(1, Map())))
  val NEG_ONE = Polynomial(Set(Term(-1, Map())))
  val ZERO = Polynomial(Set())
}

case class Polynomial(terms: Set[Term]) {
  def +(that: Polynomial): Polynomial = {
    var px = this
    that.terms.foreach { term => px = px + term }
    px
  }

  def *(that: Polynomial): Polynomial = if (isZero || that.isZero) Polynomial(Set()) else {
    var ts = Set[Term]()
    for (a <- terms; b <- that.terms)
      ts += a * b
    Polynomial(ts)
  }

  def simplify = {
    val px = Polynomial(Set()) + Polynomial(terms.map(_.simplify))
    Polynomial(px.terms.filter(!_.isZero).map(_.simplify))
  }

  override def toString = terms.size match {
    case 0 => 0.toString
    case _ => terms.map(_.toString).mkString("+")
  }

  private def isZero = terms.isEmpty || (terms.size == 1 && terms.elements.next.isZero)

  private def +(term: Term): Polynomial = {
    if (terms.filter(_.vars == term.vars).isEmpty)
      Polynomial(terms + term)
    else
      Polynomial(terms.map(t => if (t.vars == term.vars) t + term else t))
  }
}

case class Term(c: Int, vars: Map[Variable, Exponent]) {
  def +(that: Term) = Term(c + that.c, vars)

  def *(that: Term) = {
    var vs = vars
    (vars.keySet ++ that.vars.keySet).foreach { name =>
      val e = vars.getOrElse(name, 0) + that.vars.getOrElse(name, 0)
      vs += name -> e
    }
    Term(c * that.c, vs)
  }

  def isZero = c == 0

  def simplify = c match {
    case 0 => Term(0, Map())
    case _ => Term(c, vars.filter(_._2 != 0))
  }

  override def toString = vars.size match {
    case 0 => c.toString
    case _ => c + "*" + vars.map(entry => "(" + entry._1.toString.replace("'", "") + "^" + entry._2 + ")")
                                        .mkString("*")
  }
}