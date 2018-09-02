package skloibi.cfmetrics

import java.util

import javassist.CtClass
import javassist.bytecode.{CodeAttribute, MethodInfo}

import scala.collection.immutable.Stream.Empty

case class ClassAnalyzer(c: CtClass) {

  import Metrics._

  def analyze(): Unit = {
    println(c.getName)
    println("------------------------------------------")

    val metrics = Seq(
      "Fields" -> (_.getDeclaredFields.length),
      "Field attributes (median)" -> (c =>
        median {
          c.getDeclaredFields.toStream
            .map(_.getFieldInfo)
            .map(_.getAttributes.size)
            .toList
        }),
      "Field attributes (min)" -> (c =>
        c.getDeclaredFields.toStream
          .map(_.getFieldInfo)
          .map(_.getAttributes.size) match {
          case Empty    => 0
          case nonEmpty => nonEmpty.min
        }),
      "Field attributes (max)" -> (c =>
        c.getDeclaredFields.toStream
          .map(_.getFieldInfo)
          .map(_.getAttributes.size) match {
          case Empty    => 0
          case nonEmpty => nonEmpty.max
        }),
      "Constructors" -> (_.getDeclaredConstructors.length),
      "Methods" -> (_.getDeclaredMethods.length),
      "Method attributes (median)" -> (c =>
        median {
          c.getDeclaredMethods.toStream
            .map(_.getMethodInfo)
            .map(_.getAttributes.size).toList
        }),
      "Method attributes (min)" -> (c =>
        c.getDeclaredMethods.toStream
          .map(_.getMethodInfo)
          .map(_.getAttributes.size) match {
          case Empty    => 0
          case nonEmpty => nonEmpty.min
        }),
      "Method attributes (max)" -> (c =>
        c.getDeclaredMethods.toStream
          .map(_.getMethodInfo)
          .map(_.getAttributes.size) match {
          case Empty    => 0
          case nonEmpty => nonEmpty.max
        }),
      "Method length (arith. mean)" -> (c =>
        util.Arrays.stream(c.getDeclaredMethods)
          .map[MethodInfo](_.getMethodInfo)
          .map[CodeAttribute](_.getCodeAttribute)
          .filter(_ != null)
          .mapToInt(_.getCodeLength)
          .average
          .orElse(0)
        ),
      "Method length (min)" -> (c =>
        c.getDeclaredMethods.toStream
          .map(_.getMethodInfo)
          .map(_.getCodeAttribute)
          .filter(_ != null)
          .map(_.getCodeLength) match {
          case Empty    => 0
          case nonEmpty => nonEmpty.min
        }),
      "Method length (min)" -> (c =>
        c.getDeclaredMethods.toStream
          .map(_.getMethodInfo)
          .map(_.getCodeAttribute)
          .filter(_ != null)
          .map(_.getCodeLength) match {
          case Empty    => 0
          case nonEmpty => nonEmpty.max
        })
    )

    metrics.foreach(_.using(c))

  }

  private[this] def median[Num: Numeric](numbers: Seq[Num])(implicit ev: Num => Double): Double =
    numbers.length match {
      case even if even % 2 == 0 =>
        numbers.sorted.slice(even / 2 - 1, even / 2 + 1).sum / 2
      case odd                   =>
        numbers.sorted.drop(odd / 2).head
    }
}
