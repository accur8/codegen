package a8.codegen


import a8.codegen.CaseClassAst.{Annotation, CaseClass, Property}
import CommonOpsCopy._

object BuilderTemplate {

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

  lazy val jdbcMapperTemplate =
    new BuilderTemplate(
      "jdbcMapper",
      TypeName("a8.shared.jdbcf.mapper.Mapper"),
      TypeName("a8.shared.jdbcf.mapper.MapperBuilder"),
      generateFor = _.jdbcMapper,
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

      override def resolveTypeClassName(caseClass: CaseClass): String = {
        primaryKey(caseClass) match {
          case Some(pk) =>
            s"a8.shared.jdbcf.mapper.KeyedMapper[${caseClass.name},${pk.typeName}]"
          case None =>
            super.resolveTypeClassName(caseClass)
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
                s".singlePrimaryKey(_.${pkf.nameAsVal})",
                ".buildKeyedMapper"
              )
            case None =>
              List(".buildMapper")
          }

        (base + (tableName ++ suffix).mkString("\n","\n","").indent("    ")).trim

      }

    }

  lazy val qubesMapperTemplate =
    new BuilderTemplate(
      "qubesMapper",
      TypeName("a8.sync.qubes.QubesMapper"),
      TypeName("a8.sync.qubes.QubesMapperBuilder"),
      generateFor = _.qubesMapper,
    ) {

      case class QubesAnno(cube: String, appSpace: String)

      override def resolveTypeClassName(caseClass: CaseClass): String = {
        s"a8.sync.qubes.QubesKeyedMapper[${caseClass.name},${primaryKey(caseClass).typeName}]"
      }

      def qubesAnno(caseClass: CaseClass): QubesAnno =
        caseClass
          .annotations
          .find(_.name == "QubesAnno")
          .map { anno =>
            QubesAnno(
              cube = anno.parms.find(_.name == "cube").map(_.value).getOrElse('"' + caseClass.name + '"'),
              appSpace = anno.parms.find(_.name == "appSpace").map(_.value).getOrElse(sys.error("""must supply @QubesAnno(appSpace = "foo") i.e. appSpace annotation field is required""")),
            )
          }
          .getOrElse(sys.error(s"""for qubesMapper minimally the @QubesAnno(appSpace = "foo") is required for every class marked with @CompanionGen() on ${caseClass.qualifiedName}"""))

      def primaryKey(caseClass: CaseClass): Property =
        caseClass
          .properties
          .find(_.annotations.exists(_.name == "PK"))
          .getOrElse(sys.error(s"must supply PK on ${caseClass.name}"))

      override def rawBuild(caseClass: CaseClass, includeBuildCall: Boolean = true): String = {

        val qa = qubesAnno(caseClass)
        val pk = primaryKey(caseClass)

        val base = super.rawBuild(caseClass, false)
        val suffix =
          List(
            s".cubeName(${qa.cube})",
            s".appSpace(${qa.appSpace})",
            s".singlePrimaryKey(_.${pk.nameAsVal})",
            ".build",
          )

        (base + suffix.mkString("\n","\n","").indent("    ")).trim

      }

    }


  lazy val templates = List(messagePackTemplate, jdbcMapperTemplate, qubesMapperTemplate, circeTemplate, jsonObjectTemplate)

}


class BuilderTemplate(
  val valName: String,
  val typeClassName: TypeName,
  val builderClassName: TypeName,
  val generateFor: CompanionGen=>Boolean,
) {

  def build(caseClass: CaseClass): Option[String] = {
    if ( generateFor(caseClass.companionGen) ) {
      Some(rawBuild(caseClass))
    } else {
      None
    }
  }

  def resolveTypeClassName(caseClass: CaseClass): String =
    s"${typeClassName.fullName}[${caseClass.name}]"

  def rawBuild(caseClass: CaseClass, includeBuildCall: Boolean = true): String = {
    val propLines =
      caseClass
        .properties
        .map(prop => s".addField(_.${prop.nameAsVal})")
    val buildCall = includeBuildCall.option(".build")
    val body = (propLines ++ buildCall).mkString("\n").indent("    ")
    s"""
implicit lazy val ${valName}: ${resolveTypeClassName(caseClass)} =
  ${builderClassName}(generator)
${body}
""".trim
  }


}
