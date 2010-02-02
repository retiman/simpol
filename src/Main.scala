package simpol

object Main extends Parsers {
  def main(args: Array[String]) = args.foreach { expr =>
    println("Simplifying " + expr + ":")
    parseAll(sum, expr) match {
      case Success(out, int) => println("  " + out.simplify.toString)
      case ns: NoSuccess     => println("  " + ns.toString)
    }
    println()
  }
}
