package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.CodegenTemplate.TemplateFactory
import a8.codegen.CommonOpsCopy._
import a8.codegen.MoreOps._

import scala.language.postfixOps

object CodegenTemplate1 extends TemplateFactory {
}

case class CodegenTemplate1(file: java.io.File, project: Project)
  extends CodegenTemplate
{

  override val companionGenDefault: CompanionGen = CompanionGen.empty.copy(jsonFormat = true)

//  val sourceCode = scala.io.Source.fromFile("model/shared/src/main/scala/a8/manna/model/Tester2.scala").mkString
  lazy val sourceCode = {
    val s = scala.io.Source.fromFile(file)
    val sc = s.getLines().mkString("\n")
    s.close()
    sc
  }
  //====

  lazy val sf = sourceFile

  lazy val header = s"""package ${sourceFile.pakkage}

import a8.common.Lenser.{Lens, LensImpl}
import play.api.libs.json.{JsPath, Reads, OWrites}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import a8.common.CommonOps._
import a8.common.JsonAssist
import a8.common.CaseClassParm

/**

  WARNING THIS IS GENERATED CODE.  DO NOT EDIT.

  The only manually maintained code is the code between the //==== (normally where you add your imports)

*/

//====
${manualImports.mkString("\n")}
//====

"""

  case class CaseClassGen(cc: CaseClass) {

    lazy val props = cc.properties

    lazy val nonEmptyProps: Option[Iterable[CaseClassAst.Property]] = {
      props.isEmpty match {
        case true =>
          None
        case false =>
          Some(props)
      }
    }

    lazy val jsonFieldReads: String =
      props
        .map { prop =>
          val (verb, typeName) =
            if ( prop.typeName.isOption ) "readNullable" -> prop.typeName.args.head
            else "read" -> prop.typeName
          val readCall: String =
            prop
              .defaultExpr
              .map(defaultVal => s"x${verb}WithDefault[${typeName}](${defaultVal})")
              .getOrElse(s"${verb}[${typeName}]")
          s"""(JsPath \\ ${prop.nameAsStringLit}).${readCall}"""
        }
        .toList
        .mkString(" and\n")

    lazy val jsonFieldWrites: String =
      props
        .map { prop =>
          val (verb, typeName) =
            if ( prop.typeName.isOption && !cc.companionGen.writeNones ) "writeNullable" -> prop.typeName.args.head
            else "write" -> prop.typeName
          s"""(JsPath \\ ${prop.nameAsStringLit}).${verb}[${typeName}]"""
        }
        .toList
        .mkString(" and\n")

    lazy val jsonReadsWritesBody: String =
      props.size match {
        case 0 =>
          s"""
implicit lazy val jsonReads: Reads[${cc.name}] = JsonAssist.utils.lazyReads(json.emptyObjectReader(${cc.name}()))
implicit lazy val jsonWrites: OWrites[${cc.name}] = JsonAssist.utils.lazyOWrites(json.emptyObjectWriter[${cc.name}])
        """

        case 1 =>
          s"""
implicit lazy val jsonReads: Reads[${cc.name}] =
  JsonAssist.utils.lazyReads(
    ${jsonFieldReads}.map(${cc.name}.apply)
  )

implicit lazy val jsonWrites: OWrites[${cc.name}] =
  JsonAssist.utils.lazyOWrites(
    ${jsonFieldWrites}.contramap { v => v.${props.head.nameAsVal} }
  )
        """

        case _ =>
          s"""
implicit lazy val jsonReads: Reads[${cc.name}] =
  JsonAssist.utils.lazyReads((
${jsonFieldReads.indent("    ")}
  )(${cc.name}.apply _))

implicit lazy val jsonWrites: OWrites[${cc.name}] =
  JsonAssist.utils.lazyOWrites((
${jsonFieldWrites.indent("    ")}
  )(unlift(${cc.name}.unapply)))
        """
      }

    lazy val lensesBody: String =
      props
        .map { prop =>
          s"lazy val ${prop.nameAsVal}: Lens[${cc.name},${prop.typeName}] = LensImpl[${cc.name},${prop.typeName}](${prop.nameAsStringLit}, _.${prop.nameAsVal}, (d,v) => d.copy(${prop.nameAsVal} = v))"
        }
        .mkString("\n")

    lazy val parametersBody: String =
      props
        .zipWithIndex
        .map { case (prop, ordinal) =>
          s"lazy val ${prop.nameAsVal}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.nameAsStringLit}, lenses.${prop.nameAsVal}, ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
        }
        .mkString("\n")

    lazy val unsafeBody: String =
      s"""
object unsafe {

  def rawConstruct(values: IndexedSeq[Any]): ${cc.name} = {
    ${cc.name}(
${
        props
          .zipWithIndex
          .map { case (prop,i) =>
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
          .map { prop =>
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
    lazy val rpcHandler =
      s"""
  implicit val rpcHandler: a8.remoteapi.RpcHandler[${cc.name}] = {
    import a8.remoteapi.RpcHandler.RpcParm
    a8.remoteapi.RpcHandler(
      Vector(
${cc.properties.map(p => s"          RpcParm(parameters.${p.nameAsVal}),").mkString("        \n")}
      ),
      unsafe.rawConstruct,
    )
  }

"""

    lazy val bareBody = s"""
${
    if ( cc.companionGen.jsonFormat )
s"""
${jsonReadsWritesBody.trim}

lazy val jsonFormat = JsonAssist.utils.lazyFormat(Format(jsonReads, jsonWrites))
"""
    else
      ""
}
${
      if (cc.companionGen.rpcHandler)
        rpcHandler.trim
      else
        ""
}
object lenses {
${lensesBody.indent("  ")}
}

object parameters {
${parametersBody.indent("  ")}
}
${unsafeBody}

lazy val allLenses = List(${props.map(p => s"lenses.${p.nameAsVal}").mkString(",")})

lazy val allLensesHList = ${nonEmptyProps.map(_.map(p => s"lenses.${p.nameAsVal}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

lazy val allParametersHList = ${nonEmptyProps.map(_.map(p => s"parameters.${p.nameAsVal}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

lazy val typeName = "${cc.name}"

"""

      lazy val body = s"""
trait Mx${cc.name} {

${bareBody.trim.indent("  ")}

}
"""

  }

  lazy val generatedCaseClassCode = sf.caseClasses.map(CaseClassGen).map(_.body).mkString("\n\n\n")

}
