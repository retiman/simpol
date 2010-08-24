package simpol

import Polynomial._

object Term {
  implicit def intWrapper(c: Int) = Term(c)
  def apply(c: Int, factors: Pair[Symbol, Int]*): Term =
      new Term(c, Map() ++ factors)
  def apply(factors: Pair[Symbol, Int]*): Term = new Term(1, Map() ++ factors)
}

case class Term(c: Int, factors: Map[Symbol, Int]) {
  def +(that: Term) = Term(c + that.c, factors)

  def *(that: Term) = {
    var fs = factors
    factors.keySet ++ that.factors.keySet foreach { name =>
      val e = factors.getOrElse(name, 0) + that.factors.getOrElse(name, 0)
      fs += name -> e
    }
    Term(c * that.c, fs)
  }

  def simplify: Term = c match {
    case 0 => 0
    case _ => Term(c, factors.filter(_._2 != 0))
  }

  override def hashCode = c match {
    case 0 => 0.hashCode
    case _ => super.hashCode
  }

  override def equals(that: Any) = that match {
    case that: Term =>
      (c == 0 && that.c == 0) || (c == that.c && factors == that.factors)
    case _ =>
      false
  }

  override def toString = {
    implicit def pairWrapper(factor: Pair[Symbol, Int]) = new {
      def format =
          "(" + factor._1.toString.replace("'", "") + "^" + factor._2 + ")"
    }
    factors.size match {
      case 0 => c.toString
      case _ => c + "*" + factors.map(_.format).mkString("*")
    }
  }
}
