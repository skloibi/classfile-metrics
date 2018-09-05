package skloibi.cfmetrics

import java.nio.file.{Files, Path}

import javassist.{ClassPool, CtConstructor}

import scala.collection.immutable.Stream.Empty

case class ClassFileResult(path: Path) extends Result {
  val clazz = ClassPool.getDefault.makeClass(Files.newInputStream(path))

  val fields = clazz.getDeclaredFields
  val fieldInfos = fields.map(_.getFieldInfo)
  val methods = clazz.getDeclaredBehaviors
  val methodInfos = methods.map(_.getMethodInfo)

  val size: Metric = use {
    Metric("Size", "Total file size", Some("byte")) {
      Files.size(path)
    }
  }

  val nFields: Metric = use {
    Metric("#F", "Number of fields") {
      fields.length
    }
  }

  val nMethods = use {
    Metric("#M", "Number of methods (incl. constructors)") {
      methods.length
    }
  }

  val nConstructors = use {
    Metric("#C", "Number of constructors") {
      methods.count(_.isInstanceOf[CtConstructor])
    }
  }

  val nFieldAttributes = use {
    Metric("#FA", "Attributes per field (median)") {
      Analysis.median {
        fieldInfos.map(_.getAttributes.size)
      }.toDouble
    }
  }

  val fieldAttributesMin = use {
    Metric("minFA", "Minimal attributes of a field") {
      fieldInfos.map(_.getAttributes.size) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.min
      }
    }
  }

  val fieldAttributesMax = use {
    Metric("maxFA", "Maximal attributes of a field") {
      fieldInfos.map(_.getAttributes.size) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.max
      }
    }
  }

  val nMethodAttributes = use {
    Metric("#MA", "Attributes per method (median)") {
      Analysis.median {
        methodInfos.map(_.getAttributes.size)
      }.toDouble
    }
  }

  val methodAttributesMin = use {
    Metric("minMA", "Minimal attributes of a method") {
      methodInfos.map(_.getAttributes.size) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.min
      }
    }
  }

  val methodAttributesMax = use {
    Metric("maxMA", "Maximal attributes of a method") {
      methodInfos.map(_.getAttributes.size) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.max
      }
    }
  }

  val meanMethodLength = use {
    Metric("meanLen", "Mean length of a method (arithmetic mean)") {
      Analysis.mean {
        methodInfos
          .map(_.getCodeAttribute)
          .filter(_ != null)
          .map(_.getCodeLength)
      }.toDouble
    }
  }

  val minMethodLength = use {
    Metric("minLen", "Minimum length of a method") {
      methodInfos.toStream
        .map(_.getCodeAttribute)
        .filter(_ != null)
        .map(_.getCodeLength) match {
        case Empty => 0
        case other => other.min
      }
    }
  }

  val maxMethodLength = use {
    Metric("maxLen", "Maximum length of a method") {
      methodInfos.toStream
        .map(_.getCodeAttribute)
        .filter(_ != null)
        .map(_.getCodeLength) match {
        case Empty => 0
        case other => other.max
      }
    }
  }

  val totalLength = use {
    Metric("totalLen", "Total instruction length") {
      methodInfos.toStream
        .map(_.getCodeAttribute)
        .map(_.getCodeLength)
        .sum
    }
  }

}
