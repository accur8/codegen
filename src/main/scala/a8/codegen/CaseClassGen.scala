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
        s"lazy val ${prop.name}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.name.quoted}, lenses.${prop.name}, ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
      }
      .mkString("\n")

  lazy val parametersBodyTemplate2: String =
    props
      .zipWithIndex
      .map { case (prop, ordinal) =>
        s"lazy val ${prop.name}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.name.quoted}, _.${prop.name}, (d,v) => d.copy(${prop.name} = v), ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
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
          s"${prop.name} = values(${i}).asInstanceOf[${prop.typeName}],"
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
          s"${prop.name} = values.next().asInstanceOf[${prop.typeName}],"
        }
        .mkString("\n")
        .indent("        ")
}
      )
    if ( values.hasNext )
       sys.error("")
    value
  }
  def typedConstruct(${props.map(p => s"${p.name}: ${p.typeName}").mkString(", ")}): ${cc.name} =
    ${cc.name}(${props.map(_.name).mkString(", ")})

}
"""

  lazy val bareBody = s"""

object parameters {
${parametersBody.indent("  ")}
}
${unsafeBody}

lazy val allLenses = List(${props.map(p => s"lenses.${p.name}").mkString(",")})

lazy val allLensesHList = ${props.toNonEmpty.map(_.map(p => s"lenses.${p.name}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

lazy val allParametersHList = ${props.toNonEmpty.map(_.map(p => s"parameters.${p.name}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

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
      .mkString("\n")

}

lazy val generator: Generator[${cc.name},parameters.type] =  {
  val constructors = Constructors[${cc.name}](${caseClass.properties.size}, unsafe.iterRawConstruct)
  Generator(constructors, parameters)
}

object parameters {
${parametersBodyTemplate2.indent("  ")}
}

${unsafeBody}

lazy val allParametersHList = ${props.toNonEmpty.map(_.map(p => s"parameters.${p.name}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

lazy val typeName = "${cc.name}"

"""

  lazy val bodyTemplate2 = s"""
trait Mx${cc.name} {

${bareBodyTemplate2.trim.indent("  ")}

}
"""

}
