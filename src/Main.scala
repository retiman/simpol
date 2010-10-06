package simpol

object Main extends Parsers {
  def main(args: Array[String]) = args.foreach { expr =>
    println("Simplifying " + expr + ":")
    parseAll(sum, expr) match {
      case Success(result, input) => println("  " + result.simplify.toString)
      case Failure(msg, input)    => println("  " + msg.toString)
      case _                      => throw new IllegalStateException
    }
    println()
  }
}
