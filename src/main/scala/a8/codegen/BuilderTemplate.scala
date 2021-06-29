package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import CommonOpsCopy._

object BuilderTemplate {

  lazy val messagePackTemplate =
    new BuilderTemplate(
      "codec",
      "a8.wsjdbc.codec.Codec",
      "a8.wsjdbc.codec.CodecBuilder",
      generateFor = _.messagePack,
    )

  lazy val mapperTemplate =
    new BuilderTemplate(
      "mapper",
      "a8.shared.jdbcf.RowReader",
      "a8.shared.jdbcf.MapperBuilder",
      generateFor = _.rowReader,
      Some("a8.shared.jdbcf.Mapper"),
    )

  lazy val templates = List(messagePackTemplate, mapperTemplate)

}


class BuilderTemplate(
  valName: String,
  parmTypeClassName: String,
  builderClassName: String,
  generateFor: CompanionGen=>Boolean,
  finalTypeClassName: Option[String] = None,
) {

  def resolvedFinalTypeClassName = finalTypeClassName.getOrElse(parmTypeClassName)

  def build(caseClass: CaseClass): Option[String] = {
    if ( generateFor(caseClass.companionGen) ) {
      Some(rawBuild(caseClass))
    } else {
      None
    }
  }

  def rawBuild(caseClass: CaseClass): String = {
    val propLines =
      caseClass
        .properties
        .map(prop => s".addField(_.${prop.name})")
        .mkString("\n")
    s"""
implicit lazy val ${valName}: ${resolvedFinalTypeClassName}[${caseClass.name}] =
  ${builderClassName}(generator)
${propLines.indent("    ")}
    .build
""".trim
  }


}
