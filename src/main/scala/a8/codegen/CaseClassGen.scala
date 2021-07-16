package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import CommonOpsCopy._

case class CaseClassGen(caseClass: CaseClass) {

  import Codegen._

  lazy val cc: CaseClass = caseClass
  lazy val props: Iterable[CaseClassAst.Property] = caseClass.properties

  lazy val parametersBody: String =
    props
      .zipWithIndex
      .map { case (prop, ordinal) =>
        s"lazy val ${prop.nameAsVal}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.nameAsStringLit}, lenses.${prop.nameAsVal}, ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
      }
      .mkString("\n")

  lazy val parametersBodyTemplate2: String =
    props
      .zipWithIndex
      .map { case (prop, ordinal) =>
        s"lazy val ${prop.nameAsVal}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.nameAsStringLit}, _.${prop.nameAsVal}, (d,v) => d.copy(${prop.nameAsVal} = v), ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
      }
      .mkString("\n")

  lazy val unsafeBody: String =
s"""
object unsafe {

  def rawConstruct(values: IndexedSeq[Any]): ${cc.name} = {
    ${cc.name}(
${
      props
        .zipWithIndex.map { case (prop,i) =>
          s"${prop.nameAsVal} = values(${i}).asInstanceOf[${prop.typeName}],"
        }
        .mkString("\n")
        .indent("      ")
}
    )
  }
  def iterRawConstruct(values: Iterator[Any]): ${cc.name} = {
    val value =
      ${cc.name}(
${
      props
        .zipWithIndex.map { case (prop,i) =>
          s"${prop.nameAsVal} = values.next().asInstanceOf[${prop.typeName}],"
        }
        .mkString("\n")
        .indent("        ")
}
      )
    if ( values.hasNext )
       sys.error("")
    value
  }
  def typedConstruct(${props.map(p => s"${p.nameAsVal}: ${p.typeName}").mkString(", ")}): ${cc.name} =
    ${cc.name}(${props.map(_.nameAsVal).mkString(", ")})

}
"""

  lazy val bareBody = s"""

object parameters {
${parametersBody.indent("  ")}
}
${unsafeBody}

lazy val allLenses = List(${props.map(p => s"lenses.${p.nameAsVal}").mkString(",")})

lazy val allLensesHList = ${props.toNonEmpty.map(_.map(p => s"lenses.${p.nameAsVal}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

lazy val allParametersHList = ${props.toNonEmpty.map(_.map(p => s"parameters.${p.nameAsVal}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

lazy val typeName = "${cc.name}"

"""

  lazy val body = s"""
trait Mx${cc.name} {

${bareBody.trim.indent("  ")}

}
"""

  lazy val bareBodyTemplate2 = s"""

${
    BuilderTemplate
      .templates
      .flatMap(_.build(caseClass))
      .mkString("\n\n")

}

implicit val catsEq: cats.Eq[${cc.name}] = cats.Eq.fromUniversalEquals

lazy val generator: Generator[${cc.name},parameters.type] =  {
  val constructors = Constructors[${cc.name}](${caseClass.properties.size}, unsafe.iterRawConstruct)
  Generator(constructors, parameters)
}

object parameters {
${parametersBodyTemplate2.indent("  ")}
}

${unsafeBody}

lazy val typeName = "${cc.name}"

"""

  lazy val bodyTemplate2 = s"""
trait Mx${cc.name} {

${bareBodyTemplate2.trim.indent("  ")}

}
"""

}
