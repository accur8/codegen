package a8.codegen

import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}

import java.io.File
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver

import scala.language.postfixOps

object CodegenTemplate2 {

  implicit class StringOps(s: String) {
    def indent(indent: String) =
      s.linesIterator.map(indent + _).mkString("\n")
    def quoted = '\"' + s + '\"'
  }

  def isCodgenFile(f: File): Boolean = {
    if (
      f.isFile
        && f.getName.endsWith(".scala")
        && !f.getName.startsWith("Mx")
    ) {
      loadFileContents(f)
        .exists { contents =>
          contents.contains("@CompanionGen") && !contents.contains("//@NoCodegen")
        }
    } else {
      false
    }
  }

  def findCodegenScalaFiles(dir: File): IndexedSeq[File] = {
    dir.listFiles.flatMap {
      case d if d.isDirectory =>
        findCodegenScalaFiles(d)
      case f if isCodgenFile(f) =>
        IndexedSeq(f)
      case _ =>
        IndexedSeq.empty
    }
  }

  def main(args: Array[String]): Unit = {
    codeGenScalaFiles(new File("/Users/glen/code/accur8/composite/wsjdbc"))
    codeGenScalaFiles(new File("/Users/glen/code/accur8/composite/sync/ahs"))
//    codeGenScalaFiles(new File("."))
  }

  def codeGenScalaFiles(target: File): Unit = {
    println(s"finding scala files with @CompanionGen in ${target.getAbsolutePath}")
    val files = findCodegenScalaFiles(target)
    println(s"found ${files.size} scala files")

    val companionGenResolver = CompanionGen.resolver(target)

    files
      .foreach( file =>
        CodegenTemplate2(file, companionGenResolver)
          .run()
      )
  }

  def codeGenScalaFiles(): Unit =
    codeGenScalaFiles(new File("."))

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def loadFileContents(f: File): Option[String] = {
    val source = scala.io.Source.fromFile(f)
    try {
      Some(source.getLines.mkString("\n"))
    } catch {
      case e: Exception =>
        println(s"error processing ${f}")
        e.printStackTrace()
        None
    } finally {
      source.close()
    }
  }

}




case class CodegenTemplate2(file: java.io.File, companionGenResolver: CompanionGenResolver) {

  import Codegen._

  val manualImports =
    previousGeneratedSourceCode
      .linesIterator
      .toList
      .dropWhile(!_.startsWith("//===="))
      .drop(1)
      .takeWhile(!_.startsWith("//===="))

  lazy val parser = new CaseClassParser(file, companionGenResolver)(ParserConfig(true))

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

  def run(): Unit = {
    println(s"reading ${file}")
    val objectName: String = "Mx" + file.getName.split("\\.").take(1).head
    val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)
    println(s"writing ${generatedFile}")
    Codegen.printToFile(generatedFile) { out =>
      out.println(header)
      out.println(s"object ${objectName} {")
      out.println(generatedCaseClassCode.indent("  "))
      out.println("}")
    }
  }

}
