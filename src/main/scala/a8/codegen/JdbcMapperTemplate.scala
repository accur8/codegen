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
    imports =
      List(
        "import a8.shared.jdbcf.querydsl",
        "import a8.shared.jdbcf.querydsl.QueryDsl",
      )
  )
{

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
        s"a8.shared.jdbcf.mapper.ComponentMapper[${caseClass.name}]"
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
