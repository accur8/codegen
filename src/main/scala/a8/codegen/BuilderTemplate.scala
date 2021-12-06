package a8.codegen


import a8.codegen.CaseClassAst.{Annotation, CaseClass, Property, TypeName}
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

      def primaryKeys(caseClass: CaseClass): List[Property] =
        caseClass
          .properties
          .filter(_.annotations.exists(_.name == "PK"))
          .toList

      def primaryKeyLine(caseClass: CaseClass): Option[String] = {
        primaryKeys(caseClass) match {
          case Nil =>
            None
          case List(pk) =>
            Some(s".singlePrimaryKey(_.${pk.nameAsVal})")
          case l =>
            Some(s".compositePrimaryKey${l.size}(s => (${l.map(k => s"s.${k.nameAsVal}").mkString(",")}))")
        }
        //          .getOrElse(sys.error(s"must supply PK on ${caseClass.name}"))
      }

      override def resolveTypeClassName(caseClass: CaseClass): String = {
        primaryKeys(caseClass) match {
          case Nil =>
            super.resolveTypeClassName(caseClass)
          case List(pk) =>
            s"a8.shared.jdbcf.mapper.KeyedMapper[${caseClass.name},${pk.typeName}]"
          case l =>
            s"a8.shared.jdbcf.mapper.KeyedMapper[${caseClass.name},(${l.map(_.typeName).mkString(",")})]"
        }
      }

      override def rawBuild(caseClass: CaseClass, includeBuildCall: Boolean = true): String = {

        val tableName =
        sqlTableAnno(caseClass)
          .map { value =>
            s".tableName(${value})"
          }

        val base = super.rawBuild(caseClass, false)
        val suffix =
          primaryKeyLine(caseClass) match {
            case Some(pkl) =>
              List(
                pkl,
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
        primaryKeys(caseClass) match {
          case Nil =>
            sys.error("@PK is required")
          case List(pk) =>
            s"a8.sync.qubes.QubesKeyedMapper[${caseClass.name},${pk.typeName}]"
          case l =>
            s"a8.sync.qubes.QubesKeyedMapper[${caseClass.name},(${l.map(_.typeName).mkString(",")})]"
        }
      }

      def primaryKeys(caseClass: CaseClass): List[Property] =
        caseClass
          .properties
          .filter(_.annotations.exists(_.name == "PK"))
          .toList

      def qubesAnno(caseClass: CaseClass): QubesAnno =
        caseClass
          .annotations
          .find(_.name == "QubesAnno")
          .map { anno =>
            QubesAnno(
              cube = anno.parms.find(_.name == "cube").map(_.value).getOrElse('"'.toString + caseClass.name + '"'.toString),
              appSpace = anno.parms.find(_.name == "appSpace").map(_.value).getOrElse(sys.error("""must supply @QubesAnno(appSpace = "foo") i.e. appSpace annotation field is required""")),
            )
          }
          .getOrElse(sys.error(s"""for qubesMapper minimally the @QubesAnno(appSpace = "foo") is required for every class marked with @CompanionGen() on ${caseClass.qualifiedName}"""))

      def primateKeyLine(caseClass: CaseClass): String = {
        primaryKeys(caseClass) match {
          case Nil =>
            sys.error(s"must supply PK on ${caseClass.name}")
          case List(pk) =>
            s".singlePrimaryKey(_.${pk.nameAsVal})"
          case l =>
            s".compositePrimaryKey${l.size}(s => (${l.map(k => s"s.${k.nameAsVal}").mkString(",")}))"
        }
//          .getOrElse(sys.error(s"must supply PK on ${caseClass.name}"))
      }

      override def rawBuild(caseClass: CaseClass, includeBuildCall: Boolean = true): String = {

        val qa = qubesAnno(caseClass)

        val base = super.rawBuild(caseClass, false)
        val suffix =
          List(
            s".cubeName(${qa.cube})",
            s".appSpace(${qa.appSpace})",
            s"${primateKeyLine(caseClass)}",
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
