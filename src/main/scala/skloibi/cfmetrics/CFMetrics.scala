package skloibi.cfmetrics

import java.nio.file.{Files, Paths}

import javassist.ClassPool

object CFMetrics {
  val usage =
    """
    Usage: cfmetrics <filename> <filename> ...
  """

  def main(args: Array[String]) {
    val argsList = args.toList

    argsList match {
      case Nil | "-h" :: _ | "--help" :: _ => println(usage)
      case list                            =>
        val classPool = ClassPool.getDefault

        list.toStream
          .distinct
          .map(Paths.get(_))
          .filter(Files.isRegularFile(_))
          .map(Files.newInputStream(_))
          .map(classPool.makeClass)
          .map(ClassAnalyzer)
          .foreach(_.analyze())
    }

  }

}
