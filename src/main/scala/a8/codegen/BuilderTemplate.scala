package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import CommonOpsCopy._

object BuilderTemplate {

  val messagePackTemplate =
    new BuilderTemplate(
      "codec",
      "a8.wsjdbc.codec.Codec",
      "a8.wsjdbc.codec.CodecBuilder",
      generateFor = _.messagePack,
    )

  val rowReaderTemplate =
    new BuilderTemplate(
      "rowReader",
      "a8.shared.jdbcf.RowReader",
      "a8.shared.jdbcf.RowReaderBuilder",
      generateFor = _.rowReader,
    )

}


class BuilderTemplate(valName: String, typeClassName: String, builderClassName: String, generateFor: CompanionGen=>Boolean) {

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
implicit lazy val ${valName}: ${typeClassName}[${caseClass.name}] =
  ${builderClassName}(generator)
${propLines.indent("    ")}
    .build
""".trim
  }


}
