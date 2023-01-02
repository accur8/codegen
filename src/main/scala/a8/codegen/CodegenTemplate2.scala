package a8.codegen


import a8.codegen.CaseClassAst.{CaseClass, SourceFile}
import a8.codegen.CodegenTemplate.TemplateFactory
import a8.codegen.CodegenTemplate2.ResolvedCaseClass
import a8.codegen.CommonOpsCopy._
import cats.effect.{IO, IOApp}

import java.io.File
import scala.language.postfixOps
import MoreOps._

object CodegenTemplate2 extends TemplateFactory with IOApp.Simple {


  override def run: IO[Unit] = {
//    Codegen.runCodeGen(new File("c:/Users/glen/code/accur8/composite"))

//    Codegen.runCodeGen(new File("/Users/glen/code/customers/confidence/teamsync"))
//      .void

    Codegen
      .runCodeGen(new File("/Users/glen/code/accur8/composite"))
      .void

//    Codegen.runCodeGen(new File("/Users/glen/code/accur8/composite/sync"))
//      .void

//    Codegen.runCodeGen(new File("/Users/glen/code/accur8/sync"))
//      .void

//    Codegen.codeGenScalaFiles(ProjectRoot("/Users/glen/code/accur8/composite/wsjdbc"))
  }

  trait ResolvedCaseClass {
    val caseClass: CaseClass
    val model: SourceFile
    def companionGen = caseClass.companionGen
  }

}


case class CodegenTemplate2(file: java.io.File, project: Project) extends CodegenTemplate {

  override val companionGenDefault: CompanionGen = CompanionGen.empty

  val manualImports = {
    val mi =
      previousGeneratedSourceCode
        .linesIterator
        .toList
        .dropWhile(!_.startsWith("//===="))
        .drop(1)
        .takeWhile(!_.startsWith("//===="))
        .filter(_.trim.length > 0)

    if ( mi.isEmpty ) {
      List("import _root_.scala")
    } else {
      mi
    }

  }

  lazy val previousGeneratedSourceCode =
    if ( generatedFile.exists() ) {
      val s = scala.io.Source.fromFile(generatedFile)
      val gf = s.getLines().mkString("\n")
      s.close()
      gf
    } else {
      ""
    }

  lazy val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)


//  val sourceCode = scala.io.Source.fromFile("model/shared/src/main/scala/a8/manna/model/Tester2.scala").mkString
  lazy val sourceCode = {
    val s = scala.io.Source.fromFile(file)
    val sc = s.getLines().mkString("\n")
    s.close()
    sc
  }

  lazy val header = s"""package ${sourceFile.pakkage}

/**

  WARNING THIS IS GENERATED CODE.  DO NOT EDIT.

  The only manually maintained code is the code between the //==== (normally where you add your imports)

*/

//====
${manualImports.mkString("\n")}
//====

import a8.shared.Meta.{CaseClassParm, Generator, Constructors}
${
    caseClassGens
      .flatMap(ccg =>
        ccg
          .templates
          .flatMap(_.resolvedImports(ccg))
      )
      .toVector
      .distinct
      .mkString("\n")
  }

"""

  case class CaseClassGen(caseClass: CaseClass, model: SourceFile) extends ResolvedCaseClass { caseClassGen =>

    lazy val templates =
      BuilderTemplate
        .templates
        .filter(_.generateFor(caseClass.companionGen))

    lazy val props: Iterable[CaseClassAst.Property] = caseClass.properties

    lazy val parametersBody: String =
      props
        .zipWithIndex
        .map { case (prop, ordinal) =>
          s"lazy val ${prop.nameAsVal}: CaseClassParm[${caseClassName},${prop.typeName}] = CaseClassParm[${caseClassName},${prop.typeName}](${prop.nameAsStringLit}, _.${prop.nameAsVal}, (d,v) => d.copy(${prop.nameAsVal} = v), ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
        }
        .mkString("\n")

    lazy val unsafeBody: String =
      s"""
object unsafe {

  def rawConstruct(values: IndexedSeq[Any]): ${caseClassName} = {
    ${caseClassName}(
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
  def iterRawConstruct(values: Iterator[Any]): ${caseClassName} = {
    val value =
      ${caseClassName}(
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
  def typedConstruct(${props.map(p => s"${p.nameAsVal}: ${p.typeName}").mkString(", ")}): ${caseClassName} =
    ${caseClassName}(${props.map(_.nameAsVal).mkString(", ")})

}
"""

    lazy val typePerKey = {

      val result =
        caseClass
          .primaryKey
          .filter(_.typeName.fullName.endsWith(".Uid"))
          .map { pk =>
            List(
              "\n\n",
              s"type Uid = Tid[${caseClassName}]",
              s"val Uid = new Tid.Generator[${caseClassName}]",
            ).mkString("\n")
          }
          .getOrElse("")

      result

    }

    lazy val caseClassName = caseClass.name.toString

    lazy val bareBody = z"""

${
      templates
        .flatMap(_.build(caseClassGen))
        .mkString("\n\n")
}${

      typePerKey
}
${
  if (caseClass.companionGen.zio) {
    z"""
implicit val zioEq: zio.prelude.Equal[${caseClassName}] = zio.prelude.Equal.default
"""
  } else {
    ""
  }
}
implicit val catsEq: cats.Eq[${caseClassName}] = cats.Eq.fromUniversalEquals

lazy val generator: Generator[${caseClassName},parameters.type] =  {
  val constructors = Constructors[${caseClassName}](${props.size.toString}, unsafe.iterRawConstruct)
  Generator(constructors, parameters)
}

object parameters {
${parametersBody.indent("  ")}
}

${unsafeBody}

lazy val typeName = "${caseClassName}"

"""

    lazy val body = s"""
trait Mx${caseClassName} {

${bareBody.trim.indent("  ")}

}
"""

  }

  lazy val caseClassGens =
    sourceFile
      .caseClasses
      .map(cc => CaseClassGen(cc, sourceFile))

  lazy val generatedCaseClassCode =
    caseClassGens
      .map(_.body)
      .mkString("\n\n\n")

}
