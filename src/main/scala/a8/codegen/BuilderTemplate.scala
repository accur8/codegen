package a8.codegen


import a8.codegen.CaseClassAst.{Annotation, CaseClass, Property, TypeName}
import CommonOpsCopy._
import a8.codegen.CodegenTemplate2.ResolvedCaseClass

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
      callBuilderOverrideMethod = false,
    ) {

      def primaryKeyLine(caseClass: CaseClass): Option[String] = {
        caseClass.primaryKeys match {
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
        (caseClass.primaryKeys, caseClass.sqlTableAnno) match {
          case (Nil, None) =>
            s"a8.shared.jdbcf.mapper.Mapper[${caseClass.name}]"
          case (Nil, Some(_)) =>
            s"a8.shared.jdbcf.mapper.TableMapper[${caseClass.name}]"
          case (List(pk), _) =>
            s"a8.shared.jdbcf.mapper.KeyedTableMapper[${caseClass.name},${pk.typeName}]"
          case (l, _) =>
            s"a8.shared.jdbcf.mapper.KeyedTableMapper[${caseClass.name},(${l.map(_.typeName).mkString(",")})]"
        }
      }

      override def rawBuild(resolvedCaseClass: ResolvedCaseClass, includeBuildCall: Boolean = true): String = {
        import resolvedCaseClass.caseClass

        val tableName =
          caseClass
            .sqlTableAnnoValue
            .filter(_.trim.nonEmpty)
            .map { value =>
              s".tableName(${value})"
            }

        val base = super.rawBuild(resolvedCaseClass, false)
        val suffix =
          (primaryKeyLine(caseClass), tableName) match {
            case (Some(pkl), _) =>
              List(
                pkl,
                ".buildKeyedTableMapper"
              )
            case (_, Some(_)) =>
              List(".buildTableMapper")
            case _ =>
              List(".buildMapper")
          }

        val queryDsl = "\n\n" + QueryDslGenerator.generate(resolvedCaseClass)

        (base + (tableName ++ suffix).mkString("\n","\n","").indent("    ")).trim + queryDsl

      }

    }

  lazy val qubesMapperTemplate =
    new BuilderTemplate(
      "qubesMapper",
      TypeName("a8.sync.qubes.QubesMapper"),
      TypeName("a8.sync.qubes.QubesMapperBuilder"),
      generateFor = _.qubesMapper,
      callBuilderOverrideMethod = false,
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

      override def rawBuild(resolvedCaseClass: ResolvedCaseClass, includeBuildCall: Boolean = true): String = {
        import resolvedCaseClass.caseClass

        val qa = qubesAnno(caseClass)

        val base = super.rawBuild(resolvedCaseClass, false)
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
  val callBuilderOverrideMethod: Boolean = true,
) {

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
