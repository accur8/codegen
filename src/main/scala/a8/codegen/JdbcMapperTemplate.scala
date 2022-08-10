package a8.codegen


import a8.codegen.CaseClassAst.{CaseClass, TypeName}
import a8.codegen.CodegenTemplate2.ResolvedCaseClass
import CommonOpsCopy._

object
  JdbcMapperTemplate
extends
  BuilderTemplate(
    "jdbcMapper",
    TypeName("a8.shared.jdbcf.mapper.ComponentMapper"),
    TypeName("a8.shared.jdbcf.mapper.MapperBuilder"),
    generateFor = _.jdbcMapper,
    callBuilderOverrideMethod = false,
    staticImports =
      List(
        "import a8.shared.jdbcf",
      )
  )
{

  override def resolvedImports(caseClassGen: ResolvedCaseClass): Iterable[String] = {
    val asyncImport =
      if ( !caseClassGen.companionGen.zio )
        List("import cats.effect.Async")
      else
        Nil
    asyncImport ++ staticImports
  }

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
    (caseClass.primaryKeyTypeName, caseClass.sqlTableAnno) match {
      case (None, None) =>
        s"a8.shared.jdbcf.mapper.ComponentMapper[${caseClass.name}]"
      case (None, Some(_)) =>
        s"a8.shared.jdbcf.mapper.TableMapper[${caseClass.name}]"
      case (Some(pkn), _) =>
        s"a8.shared.jdbcf.mapper.KeyedTableMapper[${caseClass.name},(${pkn})]"
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

    val queryDsl = QueryDslGenerator.generate(resolvedCaseClass)

    (base + (tableName ++ suffix).mkString("\n","\n","").indent("    ")).trim + queryDsl.getOrElse("")

  }

}
