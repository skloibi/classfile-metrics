package skloibi.cfmetrics

import java.nio.file.{Files, Path}

import javassist.expr.{ConstructorCall, ExprEditor, FieldAccess, MethodCall}
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
        fields.map(_.getModifiers).map(JavassistHelper.countModifiers)
      }.toDouble
    }
  }

  val fieldAttributesMin = use {
    Metric("minFA", "Minimal attributes of a field") {
      fields.map(_.getModifiers).map(JavassistHelper.countModifiers) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.min
      }
    }
  }

  val fieldAttributesMax = use {
    Metric("maxFA", "Maximal attributes of a field") {
      fields.map(_.getModifiers).map(JavassistHelper.countModifiers) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.max
      }
    }
  }

  val nMethodAttributes = use {
    Metric("#MA", "Attributes per method (median)") {
      Analysis.median {
        methods.map(_.getModifiers).map(JavassistHelper.countModifiers)
      }.toDouble
    }
  }

  val methodAttributesMin = use {
    Metric("minMA", "Minimal attributes of a method") {
      methods.map(_.getModifiers).map(JavassistHelper.countModifiers) match {
        case a: Array[_] if a.isEmpty => 0
        case other       => other.min
      }
    }
  }

  val methodAttributesMax = use {
    Metric("maxMA", "Maximal attributes of a method") {
      methods.map(_.getModifiers).map(JavassistHelper.countModifiers) match {
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

  val totalMethodCalls = use {
    Metric("#Call", "Total method calls") {
      methods.toStream.map(m => {
        var i = 0

        m.instrument(new ExprEditor() {
          override def edit(m: MethodCall): Unit = i += 1

          override def edit(c: ConstructorCall): Unit = i += 1
        })

        i
      }).sum
    }
  }

  val totalFieldAccessLocations = use {
    Metric("#Access", "Total field access locations") {
      methods.toStream.map(m => {
        var i = 0

        m.instrument(new ExprEditor() {
          override def edit(f: FieldAccess): Unit = i += 1
        })

        i
      }).sum
    }
  }

  val totalConstPool = use {
    Metric("#CP", "Total constant pool entries") {
      clazz.getClassFile.getConstPool.getSize
    }
  }

}
