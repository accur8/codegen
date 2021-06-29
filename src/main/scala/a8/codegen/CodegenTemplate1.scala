package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}

import java.io.File
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver

import scala.language.postfixOps
import MoreOps._
import a8.codegen.CodegenTemplate.TemplateFactory

object CodegenTemplate1 extends TemplateFactory {
}

case class CodegenTemplate1(file: java.io.File, project: Project)
  extends CodegenTemplate
{

  override val companionGenDefault: CompanionGen =
    CompanionGen(
      writeNones = false,
      jsonFormat = true,
      rpcHandler = false,
      messagePack = false,
      mapper = false,
    )

  import Codegen._

  val manualImports =
    previousGeneratedSourceCode
      .linesIterator
      .toList
      .dropWhile(!_.startsWith("//===="))
      .drop(1)
      .takeWhile(!_.startsWith("//===="))

  lazy val previousGeneratedSourceCode = generatedFile.readTextOpt.getOrElse("")

  lazy val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)


//  val sourceCode = scala.io.Source.fromFile("model/shared/src/main/scala/a8/manna/model/Tester2.scala").mkString
  lazy val sourceCode = scala.io.Source.fromFile(file).getLines.mkString("\n")

  lazy val sourceFile = FastParseTools.parse(Source(sourceCode, file.getPath), parser.SourceFile)
  lazy val sf = sourceFile

  lazy val header = s"""package ${sourceFile.pakkage}

import a8.common.Lenser.{Lens, LensImpl}Lenser.{Lens, LensImpl}
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

  lazy val generatedCaseClassCode = sf.caseClasses.map(CaseClassGen).map(_.body).mkString("\n\n\n")

}
