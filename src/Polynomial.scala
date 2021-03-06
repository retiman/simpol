package simpol

import Polynomial._
import Term._

object Polynomial {
  implicit def intWrapper(c: Int) = Polynomial(c)
  def apply(c: Int) = new Polynomial(Set(Term(c)))
  def apply(factor: Pair[Symbol, Int]) = new Polynomial(Set(Term(factor)))
  def apply(terms: Term*) = new Polynomial(Set() ++ terms)
}

case class Polynomial(terms: Set[Term]) {
  assert(terms.size > 0)

  def +(that: Polynomial): Polynomial = {
    var px = this
    that.terms.foreach { term => px = px + term }
    px
  }

  def *(that: Polynomial): Polynomial = terms.size match {
    case 0 => 0
    case _ => {
      var ts = Set[Term]()
      for (a <- terms; b <- that.terms)
        ts += a * b
      Polynomial(ts)
    }
  }

  def simplify: Polynomial = {
    val px = Polynomial(terms.map(_.simplify))
    val ts = px.terms.filter(_ != Term(0))
    ts.size match {
      case 0 => 0
      case _ => Polynomial(ts.map(_.simplify))
    }
  }

  override def toString = terms.size match {
    case 0 => 0.toString
    case _ => terms.map(_.toString).mkString("+")
  }

  private def +(term: Term): Polynomial = {
    if (terms.filter(_.factors == term.factors).isEmpty)
      Polynomial(terms + term)
    else
      Polynomial(terms.map(t => if (t.factors == term.factors) t + term else t))
  }
}
