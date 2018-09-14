package skloibi.cfmetrics


object JavassistHelper {
  import javassist.Modifier._

  private def modifierChecks: Seq[Int => Boolean] = Seq(
    isAbstract,
    isAnnotation,
    isEnum,
    isFinal,
    isInterface,
    isNative,
    isPackage,
    isPrivate,
    isProtected,
    isPublic,
    isStatic,
    isStrict,
    isSynchronized,
    isTransient,
    isVarArgs,
    isVolatile,
  )

  def countModifiers(modifiers: Int): Int = {
    modifierChecks.count(_.apply(modifiers))
  }
}
