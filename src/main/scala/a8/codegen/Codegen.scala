package a8.codegen

import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}

import java.io.File
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver

import scala.language.postfixOps

object Codegen {

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

  def findProjectRoots(dir: File): IndexedSeq[ProjectRoot] = {
    dir.listFiles.flatMap {
      case d if d.isDirectory =>
        findProjectRoots(d)
      case f if f.getName == "codegen.json" =>
        IndexedSeq(ProjectRoot(dir))
      case _ =>
        IndexedSeq.empty
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

  def printHelp(args: Option[Array[String]] = None) = {

    args.map{ a =>
      println(s"a8-codegen does not support args = ${a.toList}")
      println("")
    }

    val help =
      s"""Accur8 Codegen Tool
         |
         |Usage: a8-codegen [ --help | --l-help | template1 | template2 ]
         |
         |Finds scala files in current directory with @CompanionGen and generates companion case classes
         |
         |  templateName
         |
         |  --help      shows help for a8-codegen
         |
         |  --l-help    shows the options for the app launcher (like how to update the app)
       """.stripMargin
    println(help)
  }

  def main(args: Array[String]): Unit = {

    import sys.process._

    args match {
      case Array() =>
        runCodeGen(new File("."))
      case Array("--help") =>
        printHelp()
      case Array("--l-help") =>
        "a8-codegen --l-help"!
      case _ =>
        printHelp(Some(args))
    }

  }

  def runCodeGen(dir: File): Unit =
    findProjectRoots(dir)
      .foreach(codeGenScalaFiles)


  def codeGenScalaFiles(projectRoot: ProjectRoot): Unit = {
    val project = Project(projectRoot)

    val templateFactory = CodegenTemplate(project.config.template)

    println(s"finding scala files with @CompanionGen in ${projectRoot}")
    val files = findCodegenScalaFiles(projectRoot.dir.toFile)
    println(s"found ${files.size} scala files")

    files
      .foreach( file =>
        templateFactory(file, project).run()
      )
  }

//  def codeGenScalaFiles(): Unit =
//    codeGenScalaFiles(new File("."))

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

