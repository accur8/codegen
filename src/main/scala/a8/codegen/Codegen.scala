package a8.codegen

import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}

import java.io.File
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver

import scala.language.postfixOps

object Codegen {

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
      case Array("template1") =>
        CodegenTemplate2.codeGenScalaFiles(new File("."))
      case Array("template2") =>
        CodegenTemplate2.codeGenScalaFiles(new File("."))
      case Array("--help") =>
        printHelp()
      case Array("--l-help") =>
        "a8-codegen --l-help"!
      case _ =>
        printHelp(Some(args))
    }

  }

  def codeGenScalaFiles(target: File): Unit = {
    val companionGenResolver = CompanionGen.resolver(target)
    println(s"finding scala files with @CompanionGen in ${target.getAbsolutePath}")
    val files = findCodegenScalaFiles(target)
    println(s"found ${files.size} scala files")

    files
      .foreach( file =>
        Codegen(file, companionGenResolver)
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




case class Codegen(file: java.io.File, companionGenResolver: CompanionGenResolver) {

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
  if ( caseClass.companionGen.messagePack ) {
s"""
implicit lazy val codec: a8.wsjdbc.codec.Codec[${cc.name}] =
  a8.wsjdbc.codec.CodecBuilder(generator)
${
  val propLines =
    caseClass
      .properties
      .map(prop => s".addField(_.${prop.name})")
  (propLines ++ Iterable(".build"))
    .mkString("\n")
    .indent("    ")
}
""".trim
  } else {
    ""
  }
}${
  if ( caseClass.companionGen.rowReader ) {
s"""
implicit lazy val rowReader: a8.shared.jdbcf.RowReader[${cc.name}] =
  a8.shared.jdbcf.RowReaderBuilder(generator)
${
  val propLines =
    caseClass
      .properties
      .map(prop => s".addField(_.${prop.name})")
  (propLines ++ Iterable(".build"))
    .mkString("\n")
    .indent("    ")
}
""".trim
  } else {
    ""
  }
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
