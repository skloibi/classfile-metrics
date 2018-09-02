package skloibi.cfmetrics

import javassist.CtClass

object Metrics {

  type MetricValue = AnyVal

  type ClassMetric = (String, CtClass => MetricValue)

  trait Metric {
    self =>

    def name: String

    def unit: Option[String]

    def value: CtClass => MetricValue

    def using(clazz: CtClass): Unit =
      unit match {
        case Some(unit) => println(s"$name: ${self.value(clazz)} $unit")
        case _          => println(s"$name: ${self.value(clazz)}")
      }
  }

  implicit class MetricBuilderWithUnit(nameUnit: (String, String)) {

    def ->(f: CtClass => MetricValue): Metric =
      new Metric {
        override def name: String = nameUnit._1

        override def unit: Option[String] =
          Some(nameUnit._2).filter(!_.isEmpty)

        override def value: CtClass => MetricValue = f
      }
  }

  implicit class MetricBuilder(name: String) extends MetricBuilderWithUnit((name, ""))

}
