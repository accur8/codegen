package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import Codegen.StringOps

object BuilderTemplate {

  val messagePackTemplate =
    new BuilderTemplate(
      "a8.wsjdbc.codec.Codec",
      "a8.wsjdbc.codec.CodecBuilder",
      generateFor = _.messagePack,
    )

  val rowReaderTemplate =
    new BuilderTemplate(
      "a8.shared.jdbcf.RowReader",
      "a8.shared.jdbcf.RowReaderBuilder",
      generateFor = _.rowReader,
    )


class BuilderTemplate(typeClassName: String, builderClassName: String, generateFor: CompanionGen=>Boolean) {

  val typeClassNameShort = 

  def build(caseClass: CaseClass): String = {
    s"""
implicit lazy val codec: ${typeClassName}[${cc.name}] =
  ${builderClassName}(generator)
${
    val propLines =
      caseClass
        .properties
        .map(prop => s".addField(_.${prop.name})")
    (propLines ++ Iterable(".build"))
      .mkString("\n")
      .indent("    ")
  }
""".trim
  }

}
