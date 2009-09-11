package simpol

import Polynomial._
import Term._

object Polynomial {
  val ONE = Polynomial(Set(Term(1)))
  val NEG_ONE = Polynomial(Set(Term(-1)))
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
    if (terms.filter(_.factors == term.factors).isEmpty)
      Polynomial(terms + term)
    else
      Polynomial(terms.map(t => if (t.factors == term.factors) t + term else t))
  }
}