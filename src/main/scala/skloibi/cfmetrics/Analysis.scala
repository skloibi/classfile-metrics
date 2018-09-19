package skloibi.cfmetrics

import java.text.DecimalFormat

object Analysis {
  def median[T](numbers: Seq[T])(implicit ev: Integral[T]): T =
    numbers.length match {
      case even if even % 2 == 0 =>
        ev.quot(numbers.sorted.slice(even / 2 - 1, even / 2 + 1).sum, ev.fromInt(2))
      case odd                   =>
        numbers.sorted.drop(odd / 2).head
    }

  def mean[T](numbers: Seq[T])(implicit ev: Integral[T]): T =
    ev.quot(numbers.sum, ev.fromInt(numbers.length))

  val df = new DecimalFormat("# ###.##;-#")

  def format(d: Double): String =
    df.format(d)

  def evaluatePairs(a: ClassFileResult)(b: ClassFileResult): Unit = {
    println(a.clazz.getName)
    println(b.clazz.getName)
    Stream(a.metrics, b.metrics)
      .flatMap(_.keys)
      .sorted
      .distinct
      .map(k => (a.metrics.get(k), b.metrics.get(k)))
      .foreach {
        p =>
          println {
            p match {
              case (Some(m1), Some(m2)) =>
                "%10s | %10s %10s | %10s (%s%%)".format(
                  m1.code,
                  format(m1.v),
                  format(m2.v),
                  format(m2.v - m1.v),
                  format(if (m1.v != 0) m2.v / m1.v * 100 else 100.0)
                )
              case (Some(m1), None)     =>
                "%10s | %10s      -      | %10s (%s%%)".format(
                  m1.code,
                  format(m1.v),
                  format(-m1.v),
                  format(0.0)
                )
              case (None, Some(m2))     =>
                "%10s |      -      %10s | %10s (%s%%)".format(
                  m2.code,
                  format(m2.v),
                  format(m2.v),
                  format(100.0)
                )
            }
          }
      }

  }
}
