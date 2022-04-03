package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
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

  def generate(caseClass: CaseClass): String = {
    // get @Join annos

    val fields =
      caseClass
        .properties
        .map(p => s"  val ${p.nameAsVal} = QueryDsl.field[${p.typeName.fullName}](${p.nameAsStringLit}, join)")
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
lazy val ${joinAnno.name.stripQuotes}: TableDsl = {
  val childJoin = QueryDsl.createJoin(join, ${joinAnno.name}, queryDsl.tableDsl, ()=>container, ${joinAnno.to.stripQuotes}.jdbcMapper) { (from,to) =>
    ${joinAnno.expr.stripQuotes}
  }
  new TableDsl(childJoin)
}
"""
        }
        .mkString("\n")

    toString

    s"""
class TableDsl(join: QueryDsl.Join = QueryDsl.RootJoin) {
${fields}
${joins.indent("  ")}
}

val queryDsl = new QueryDsl[${caseClass.name.value}, TableDsl](jdbcMapper, new TableDsl)

def query[F[_]: cats.effect.Async](whereFn: TableDsl => QueryDsl.Condition): SelectQuery[F, ${caseClass.name.value}, TableDsl] =
  queryDsl.query(whereFn)

def update[F[_]: cats.effect.Async](set: TableDsl => Iterable[UpdateQuery.Assignment[_]]): UpdateQuery[F, TableDsl] =
  queryDsl.update(set)

"""

  }

}
