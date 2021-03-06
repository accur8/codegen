package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import CommonOpsCopy._
import a8.codegen.CodegenTemplate2.ResolvedCaseClass
import MoreOps._

object QueryDslGenerator {

/*

    class TableDsl(join: Join = QueryDsl.RootJoin) {
      val id = QueryDsl.field[String]("id", join)
      val name = QueryDsl.field[String]("name", join)
      val containerId = QueryDsl.field[String]("containerId", join)
      lazy val container: Container.TableDsl = {
        val childJoin = QueryDsl.createJoin(join, "container", queryDsl.tableDsl, ()=>container, Container.jdbcMapper) { (from,to) =>
          from.containerId === to.id
        }
        new Container.TableDsl(childJoin)
      }
    }

    val queryDsl = new QueryDsl[Widget, TableDsl](jdbcMapper, new TableDsl)

    def query(whereFn: TableDsl => QueryDsl.Condition): SelectQuery[Widget, TableDsl] =
      queryDsl.query(whereFn)

    def update(set: TableDsl => Iterable[UpdateQuery.Assignment[_]]): UpdateQuery[TableDsl] =
      queryDsl.update(set)

 */

  case class JoinAnno(
    name: String,
    expr: String,
    to: String,
  )

  def generate(resolvedCaseClass: ResolvedCaseClass): String = {
    import resolvedCaseClass.caseClass
    // get @Join annos

    val fields =
      caseClass
        .properties
        .map { prop =>
          resolvedCaseClass.model.caseClassesByName.get(prop.typeName.fullName) match {
            case None =>
              s"  val ${prop.nameAsVal} = querydslp.QueryDsl.field[${prop.typeName.fullName}](${prop.nameAsStringLit}, join)"
            case Some(cc) =>
              s"""  val ${prop.nameAsVal} = new ${cc.name.value}.TableDsl(querydslp.QueryDsl.ComponentJoin(${prop.nameAsStringLit}, join))"""
          }
        }
        .mkString("\n")


    val joins =
      caseClass
        .annotations
        .filter(_.name == "JoinTo")
        .map { anno =>
          def parm(name: String): String = anno.parms.find(_.name == name).map(_.value).getOrElse(sys.error(s"unable to find field ${name} in @JoinTo anno"))
          JoinAnno(
            name = parm("name"),
            expr = parm("expr"),
            to = parm("to"),
          )
        }
        .map { joinAnno =>
s"""
lazy val ${joinAnno.name.stripQuotes}: ${joinAnno.to.stripQuotes}.TableDsl = {
  val childJoin = querydslp.QueryDsl.createJoin(join, ${joinAnno.name}, queryDsl.tableDsl, join=>new ${joinAnno.to.stripQuotes}.TableDsl(join), ${joinAnno.to.stripQuotes}.jdbcMapper) { (from,to) =>
    ${joinAnno.expr.stripQuotes}
  }
  new ${joinAnno.to.stripQuotes}.TableDsl(childJoin)
}
"""
        }
        .mkString("\n")

    val queryMethods =
      if ( resolvedCaseClass.caseClass.hasSqlTable ) {

        val fParameter =
          if ( resolvedCaseClass.companionGen.zio )
            ""
          else
            "F, "

        val fBracketParameter =
          if ( resolvedCaseClass.companionGen.zio )
            ""
          else
            "[F]"

        val typeParameters =
          if ( resolvedCaseClass.companionGen.zio )
            s"${caseClass.name.value}, TableDsl"
          else
            s"${caseClass.name.value}, TableDsl"

        val keyParameter = {
          resolvedCaseClass.companionGen.zio match {
//            case true =>
//              ""
//            case false =>
            case _ =>
              z", ${resolvedCaseClass.caseClass.primaryKeyTypeName.getOrElse("Unit")}"
          }
        }

        val methodTypeParameters =
          if ( resolvedCaseClass.companionGen.zio )
            ""
          else
            s"[F[_]: Async]"

        //        val queryDsl = new QueryDsl[${caseClass.name.value}, TableDsl, ${caseClass.primaryKeyTypeName.getOrElse("Unit")}](jdbcMapper, new TableDsl)
z"""
val queryDsl = new querydslp.QueryDsl[${typeParameters}${keyParameter}](jdbcMapper, new TableDsl)

def query${methodTypeParameters}(whereFn: TableDsl => querydslp.QueryDsl.Condition): querydslp.SelectQuery[${fParameter}${typeParameters}] =
  queryDsl.query${fBracketParameter}(whereFn)

def update${methodTypeParameters}(set: TableDsl => Iterable[querydslp.UpdateQuery.Assignment[_]]): querydslp.UpdateQuery[${fParameter}TableDsl] =
  queryDsl.update${fBracketParameter}(set)
"""
      } else {
        ""
      }

    val tableDslClassDefLine =
      if ( resolvedCaseClass.caseClass.hasSqlTable ) {
        s"class TableDsl(join: querydslp.QueryDsl.Join = querydslp.QueryDsl.RootJoin) {"
      } else {
        s"class TableDsl(join: querydslp.QueryDsl.Path) extends querydslp.QueryDsl.Component[${caseClass.name.value}](join) {"
      }


    s"""
${tableDslClassDefLine}
${fields}
${joins.indent("  ")}
}
${queryMethods}
"""

  }

}
