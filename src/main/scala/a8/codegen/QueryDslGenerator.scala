package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import CommonOpsCopy._
import a8.codegen.CodegenTemplate2.ResolvedCaseClass

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

    def query[F[_]: Async](whereFn: TableDsl => QueryDsl.Condition): SelectQuery[F, Widget, TableDsl] =
      queryDsl.query(whereFn)

    def update[F[_]: Async](set: TableDsl => Iterable[UpdateQuery.Assignment[_]]): UpdateQuery[F, TableDsl] =
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
              s"  val ${prop.nameAsVal} = QueryDsl.field[${prop.typeName.fullName}](${prop.nameAsStringLit}, join)"
            case Some(cc) =>
              s"""  val ${prop.nameAsVal} = new ${cc.name.value}.TableDsl(QueryDsl.ComponentJoin(${prop.nameAsStringLit}, join))"""
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
  val childJoin = QueryDsl.createJoin(join, ${joinAnno.name}, queryDsl.tableDsl, join=>new ${joinAnno.to.stripQuotes}.TableDsl(join), ${joinAnno.to.stripQuotes}.jdbcMapper) { (from,to) =>
    ${joinAnno.expr.stripQuotes}
  }
  new ${joinAnno.to.stripQuotes}.TableDsl(childJoin)
}
"""
        }
        .mkString("\n")

    val queryMethods =
      if ( resolvedCaseClass.caseClass.hasSqlTable ) {
s"""
val queryDsl = new QueryDsl[${caseClass.name.value}, TableDsl](jdbcMapper, new TableDsl)

def query[F[_]: cats.effect.Async](whereFn: TableDsl => QueryDsl.Condition): querydsl.SelectQuery[F, ${caseClass.name.value}, TableDsl] =
  queryDsl.query(whereFn)

def update[F[_]: cats.effect.Async](set: TableDsl => Iterable[querydsl.UpdateQuery.Assignment[_]]): querydsl.UpdateQuery[F, TableDsl] =
  queryDsl.update(set)
"""
      } else {
        ""
      }

    val tableDslClassDefLine =
      if ( resolvedCaseClass.caseClass.hasSqlTable ) {
        s"class TableDsl(join: QueryDsl.Join = QueryDsl.RootJoin) {"
      } else {
        s"class TableDsl(join: QueryDsl.Linker) extends QueryDsl.Component[${caseClass.name.value}](join) {"
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
