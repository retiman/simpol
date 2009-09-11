package simpol

import Polynomial._

object Term {
  def apply(c: Int) = new Term(c, Map())
  def apply(c: Int, factor: Pair[Symbol, Int]) = new Term(c, Map(factor))
}

case class Term(c: Int, factors: Map[Symbol, Int]) {
  def +(that: Term) = Term(c + that.c, factors)

  def *(that: Term) = {
    var fs = factors
    (factors.keySet ++ that.factors.keySet).foreach { name =>
      val e = factors.getOrElse(name, 0) + that.factors.getOrElse(name, 0)
      fs += name -> e
    }
    Term(c * that.c, fs)
  }

  def isZero = c == 0

  def simplify = c match {
    case 0 => Term(0)
    case _ => Term(c, factors.filter(_._2 != 0))
  }

  override def toString = factors.size match {
    case 0 => c.toString
    case _ => c + "*" + factors.map(entry => "(" + entry._1.toString.replace("'", "") + "^" + entry._2 + ")")
                                        .mkString("*")
  }
}