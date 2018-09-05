package skloibi.cfmetrics

import java.nio.file.Paths

object CFMetrics {
  val usage =
    """
    Usage: cfmetrics <filename> <filename> ...
  """

  def main(args: Array[String]) {
    val argsList = args.toList

    argsList match {
      case Nil | "-h" :: _ | "--help" :: _ => println(usage)
      case a :: b :: _                     =>
        Analysis.evaluatePairs(ClassFileResult(Paths.get(a)))(ClassFileResult(Paths.get(b)))
    }

  }

}
