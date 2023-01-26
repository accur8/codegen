package a8.codegen


import java.io.File

import CommonOpsCopy._
import a8.codegen.CaseClassAst.{CaseClassName, SourceFile}
import a8.codegen.Codegen.{CodeGenFailure, CodeGenResult, CodeGenSuccess}
import a8.codegen.CompanionGen.CompanionGenResolver
import cats.effect.IO
import MoreOps._

import scala.util.control.NonFatal

object CodegenTemplate {

  def apply(name: String): TemplateFactory =
    name.toLowerCase match {
      case "template1" =>
        CodegenTemplate1
      case "template2" =>
        CodegenTemplate2
    }

  trait TemplateFactory {
    def apply(file: java.io.File, project: Project): CodegenTemplate
  }

}


trait CodegenTemplate {

  val project: Project
  val companionGenDefault: CompanionGen
  val generatedCaseClassCode: String
  val header: String
  val file: File

  lazy val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)

  val manualImports = {
    val mi =
      previousGeneratedSourceCode
        .linesIterator
        .toList
        .dropWhile(!_.startsWith("//===="))
        .drop(1)
        .takeWhile(!_.startsWith("//===="))
        .filter(_.trim.length > 0)

    if (mi.isEmpty) {
      List(
        "// noop import so IDE generated imports get put inside the comments block, this can be removed once you have at least one other import",
        "import _root_.scala",
      )
    } else {
      mi
    }

  }
  lazy val previousGeneratedSourceCode = generatedFile.readTextOpt.getOrElse("")

  lazy val sourceFile: SourceFile = ScalaMetaParser.parseSourceFile(file, resolveCompanionGen)

  def resolveCompanionGen(caseClassName: CaseClassName, sourceAnno: a8.codegen.ProjectConfig.Anno): CompanionGen =
    project
      .companionGenResolver
      .resolve(caseClassName, file, sourceAnno, companionGenDefault)

  def run(): IO[CodeGenResult] = {
    IO.blocking {
      try {
        val objectName: String = "Mx" + file.getName.split("\\.").take(1).head
        val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)
        Codegen.printToFile(generatedFile) { out =>
          out.println(header)
          out.println(s"object ${objectName} {")
          out.println(generatedCaseClassCode.indent("  "))
          out.println("}")
        }
        CodeGenSuccess(file, generatedFile)
      } catch {
        case NonFatal(th) =>
          CodeGenFailure(Some(file), th)
      }
    }
  }

}
