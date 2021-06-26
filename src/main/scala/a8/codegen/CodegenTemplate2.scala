package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}

import java.io.File
import CommonOpsCopy._
import a8.codegen.CodegenTemplate.TemplateFactory
import a8.codegen.CompanionGen.CompanionGenResolver

import scala.language.postfixOps

object CodegenTemplate2 extends TemplateFactory {

  def main(args: Array[String]): Unit = {
    Codegen.runCodeGen(new File("/Users/glen/code/accur8/composite"))
//    Codegen.codeGenScalaFiles(ProjectRoot("/Users/glen/code/accur8/composite/sync"))
//    Codegen.codeGenScalaFiles(ProjectRoot("/Users/glen/code/accur8/composite/wsjdbc"))
  }

}


case class CodegenTemplate2(file: java.io.File, project: Project) extends CodegenTemplate {

  import Codegen._


  override val companionGenDefault: CompanionGen =
    CompanionGen(
      writeNones = false,
      jsonFormat = false,
      rpcHandler = false,
      messagePack = true,
      rowReader = false,
    )

  val manualImports =
    previousGeneratedSourceCode
      .linesIterator
      .toList
      .dropWhile(!_.startsWith("//===="))
      .drop(1)
      .takeWhile(!_.startsWith("//===="))

  lazy val previousGeneratedSourceCode =
    if ( generatedFile.exists() )
      scala.io.Source.fromFile(generatedFile).getLines.mkString("\n")
    else
      ""

  lazy val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)


//  val sourceCode = scala.io.Source.fromFile("model/shared/src/main/scala/a8/manna/model/Tester2.scala").mkString
  lazy val sourceCode = scala.io.Source.fromFile(file).getLines.mkString("\n")

  lazy val sourceFile = FastParseTools.parse(Source(sourceCode, file.getPath), parser.SourceFile)
  lazy val sf = sourceFile

  lazy val header = s"""package ${sourceFile.pakkage}

import a8.shared.Meta.{CaseClassParm, Generator, Constructors}

/**

  WARNING THIS IS GENERATED CODE.  DO NOT EDIT.

  The only manually maintained code is the code between the //==== (normally where you add your imports)

*/

//====
${manualImports.mkString("\n")}
//====

"""

  lazy val generatedCaseClassCode = sf.caseClasses.map(CaseClassGen).map(_.bodyTemplate2).mkString("\n\n\n")

}
