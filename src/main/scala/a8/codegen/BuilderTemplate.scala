package a8.codegen


import a8.codegen.CaseClassAst.{Annotation, CaseClass, Property, TypeName}
import CommonOpsCopy._
import a8.codegen.CodegenTemplate2.ResolvedCaseClass

object BuilderTemplate {

  case class QubesAnno(cube: String, appSpace: String)

  lazy val messagePackTemplate =
    new BuilderTemplate(
      "messagePackCodec",
      TypeName("a8.wsjdbc.codec.Codec"),
      TypeName("a8.wsjdbc.codec.CodecBuilder"),
      generateFor = _.messagePack,
    )

  lazy val circeTemplate =
    new BuilderTemplate(
      "circeCodec",
      TypeName("io.circe.Codec"),
      TypeName("a8.shared.CirceCodecBuilder"),
      generateFor = _.circeCodec,
    )

  lazy val jsonObjectTemplate =
    new BuilderTemplate(
      "jsonCodec",
      TypeName("a8.shared.json.JsonTypedCodec"),
      TypeName("a8.shared.json.JsonObjectCodecBuilder"),
      generateFor = _.jsonCodec,
    ) {

      override def resolveTypeClassName(caseClass: CaseClass): String =
        s"${typeClassName.fullName}[${caseClass.name},a8.shared.json.ast.JsObj]"

    }


  lazy val templates = List(messagePackTemplate, JdbcMapperTemplate, QubesMapperTemplate, circeTemplate, jsonObjectTemplate)

}


class BuilderTemplate(
  val valName: String,
  val typeClassName: TypeName,
  val builderClassName: TypeName,
  val generateFor: CompanionGen=>Boolean,
  val callBuilderOverrideMethod: Boolean = true,
  val staticImports: Iterable[String] = Iterable.empty,
) {

  def resolvedImports(caseClassGen: ResolvedCaseClass): Iterable[String] =
    staticImports

  def build(caseClass: ResolvedCaseClass): Option[String] = {
    if ( generateFor(caseClass.companionGen) ) {
      Some(rawBuild(caseClass))
    } else {
      None
    }
  }

  def resolveBuilderClassName(caseClass: CaseClass): String =
    s"${builderClassName.fullName}[${caseClass.name},parameters.type]"

  def resolveTypeClassName(caseClass: CaseClass): String =
    s"${typeClassName.fullName}[${caseClass.name}]"

  def rawBuild(resolvedCaseClass: ResolvedCaseClass, includeBuildCall: Boolean = true): String = {
    import resolvedCaseClass.caseClass

    val builderOverrideMethodName =
      s"${valName}Builder"

    val callBuilderOverrideLines =
      if ( callBuilderOverrideMethod ) {
        val builderClassName = resolveBuilderClassName(caseClass)
        Vector(s"protected def ${builderOverrideMethodName}(builder: ${builderClassName}): ${builderClassName} = builder","")
      } else {
        Vector.empty
      }

    val rawPropLines = (
      Vector(s"${builderClassName}(generator)") ++
        caseClass
          .properties
          .map(prop => s"  .addField(_.${prop.nameAsVal})")
    )

    val propLines =
      if ( callBuilderOverrideMethod ) {
        Vector(s"${builderOverrideMethodName}(") ++ rawPropLines.map("  " + _) ++ Vector(")")
      } else {
        rawPropLines
      }

    val buildCall = includeBuildCall.option(".build")

    val headerLines: Seq[String] = callBuilderOverrideLines ++ Vector(s"implicit lazy val ${valName}: ${resolveTypeClassName(caseClass)} =")
    val bodyLines = (propLines ++ buildCall).map("  " + _)

    (headerLines ++ bodyLines).mkString("\n")

  }

}
