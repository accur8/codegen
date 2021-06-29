package a8.codegen


import a8.codegen.CaseClassAst.{Annotation, CaseClass, Property}
import CommonOpsCopy._

object BuilderTemplate {

  lazy val messagePackTemplate =
    new BuilderTemplate(
      "codec",
      TypeName("a8.wsjdbc.codec.Codec"),
      TypeName("a8.wsjdbc.codec.CodecBuilder"),
      generateFor = _.messagePack,
    )

  lazy val mapperTemplate =
    new BuilderTemplate(
      "mapper",
      TypeName("a8.shared.jdbcf.mapper.Mapper"),
      TypeName("a8.shared.jdbcf.mapper.MapperBuilder"),
      generateFor = _.mapper,
    ) {

      def sqlTableAnno(caseClass: CaseClass): Option[String] =
        caseClass
          .annotations
          .find(_.name == "SqlTable")
          .map(_.parms.head.value)

      def primaryKey(caseClass: CaseClass): Option[Property] =
        caseClass
          .properties
          .find(_.annotations.exists(_.name == "PK"))

      override def typeClassName(caseClass: CaseClass): String = {
        primaryKey(caseClass) match {
          case Some(pk) =>
            s"a8.shared.jdbcf.mapper.KeyedMapper[${caseClass.name},${pk.typeName}]"
          case None =>
            super.typeClassName(caseClass)
        }
      }

      override def rawBuild(caseClass: CaseClass, includeBuildCall: Boolean = true): String = {

        val primaryKeyField = primaryKey(caseClass)

        val tableName =
        sqlTableAnno(caseClass)
          .map { value =>
            s".tableName(${value})"
          }

        val base = super.rawBuild(caseClass, false)
        val suffix =
          primaryKeyField match {
            case Some(pkf) =>
              List(
                s".singlePrimaryKey(_.${pkf.name})",
                ".buildKeyedMapper"
              )
            case None =>
              List(".buildMapper")
          }

        (base + (tableName ++ suffix).mkString("\n","\n","").indent("    ")).trim

      }

    }

  lazy val templates = List(messagePackTemplate, mapperTemplate)

}


class BuilderTemplate(
  valName: String,
  typeClassName: TypeName,
  builderClassName: TypeName,
  generateFor: CompanionGen=>Boolean,
) {

  def build(caseClass: CaseClass): Option[String] = {
    if ( generateFor(caseClass.companionGen) ) {
      Some(rawBuild(caseClass))
    } else {
      None
    }
  }

  def typeClassName(caseClass: CaseClass): String =
    s"${typeClassName.fullName}[${caseClass.name}]"

  def rawBuild(caseClass: CaseClass, includeBuildCall: Boolean = true): String = {
    val propLines =
      caseClass
        .properties
        .map(prop => s".addField(_.${prop.name})")
    val buildCall = includeBuildCall.option(".build")
    val body = (propLines ++ buildCall).mkString("\n").indent("    ")
    s"""
implicit lazy val ${valName}: ${typeClassName(caseClass)} =
  ${builderClassName}(generator)
${body}
""".trim
  }


}
