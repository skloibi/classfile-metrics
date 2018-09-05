package skloibi.cfmetrics

case class Metric(code: String, name: String, unit: Option[String] = None)
                 (value: => Double) {
  self =>

  lazy val v: Double = value

  override def hashCode(): Int = code.hashCode

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case m: Metric => self.name == m.name
      case _         => false
    }
}
