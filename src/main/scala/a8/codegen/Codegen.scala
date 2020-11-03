package a8.codegen

import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}
import java.io.File
import CommonOpsCopy._
import scala.language.postfixOps

object Codegen {

  implicit class StringOps(s: String) {
    def indent(indent: String) =
      s.lines.map(indent + _).mkString("\n")
    def quoted = '\"' + s + '\"'
  }

  def isCodgenFile(f: File): Boolean = {
    if (
      f.isFile
        && f.getName.endsWith(".scala")
        && !f.getName.startsWith("Mx")
    ) {
      val contents = loadFileContents(f)
      contents.exists(_.contains("@CompanionGen"))
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
         |Usage: a8-codegen [] [--help] [--l-help]
         |
         |Finds scala files in current directory with @CompanionGen and generates companion case classes
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
        codeGenScalaFiles(new File("/Users/glen/code/accur8/odin/remoteapi"))
      case Array(oneArg) => oneArg match {
        case "--help" =>
          printHelp()
        case "--l-help" =>
          "a8-codegen --l-help"!
        case _ =>
          printHelp(Some(args))
      }
      case _ =>
        printHelp(Some(args))
    }

  }

  def codeGenScalaFiles(target: File): Unit = {
    println("finding scala files with @CompanionGen")
    val files = findCodegenScalaFiles(target)
    println(s"found ${files.size} scala files")

    files
      .foreach( file =>
        Codegen(file)
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




case class Codegen(file: java.io.File) {

  import Codegen._

  val manualImports =
    previousGeneratedSourceCode
      .lines
      .toList
      .dropWhile(!_.startsWith("//===="))
      .drop(1)
      .takeWhile(!_.startsWith("//===="))

  lazy val parser = new CaseClassParser()(ParserConfig(true))

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

  lazy val generatedCaseClassCode = sf.caseClasses.map(CaseClassGen).map(_.body).mkString("\n\n\n")

  def run(): Unit = {
    println(s"reading ${file}")
    val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)
    println(s"writing ${generatedFile}")
    Codegen.printToFile(generatedFile) { out =>
      out.write(header)
      out.write(generatedCaseClassCode)
    }
  }

}

// this matches the CompanionGen annno in a8.common
case class CompanionGen(writeNones: Boolean = false, jsonFormat: Boolean = true, rpcHandler: Boolean = false)

case class CaseClassGen(caseClass: CaseClass) {

  import Codegen._

  lazy val cc: CaseClass = caseClass
  lazy val props: Iterable[CaseClassAst.Property] = caseClass.properties

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
        s"""(JsPath \\ "${prop.name}").${readCall}"""
      }
      .toList
      .mkString(" and\n")

  lazy val jsonFieldWrites: String =
    props
      .map { prop =>
        val (verb, typeName) =
          if ( prop.typeName.isOption && !caseClass.companionGen.writeNones ) "writeNullable" -> prop.typeName.args.head
          else "write" -> prop.typeName
        s"""(JsPath \\ "${prop.name}").${verb}[${typeName}]"""
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
    ${jsonFieldWrites}.contramap { v => v.${props.head.name} }
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
        s"lazy val ${prop.name}: Lens[${cc.name},${prop.typeName}] = LensImpl[${cc.name},${prop.typeName}](${prop.name.quoted}, _.${prop.name}, (d,v) => d.copy(${prop.name} = v))"
      }
      .mkString("\n")

  lazy val parametersBody: String =
    props
      .zipWithIndex
      .map { case (prop, ordinal) =>
        s"lazy val ${prop.name}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.name.quoted}, lenses.${prop.name}, ${prop.defaultExpr}, ${ordinal})"
      }
      .mkString("\n")

  lazy val rpcHandlerBody: String =
    if ( caseClass.companionGen.rpcHandler )
s"""
implicit val rpcHandler: a8.remoteapi.RpcHandler[${cc.name}] = {
  import a8.remoteapi.RpcHandler.RpcParm
  a8.remoteapi.RpcHandler(
    Vector(
${props.map(prop => s"RpcParm(parameters.${prop.name})").mkString(",\n").indent("      ")}
    ),
    unsafe.rawConstruct,
  )
}
"""
    else
      ""

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
        .indent("    ")
}
    )
  }
}
"""

  lazy val bareBody = s"""
${
    if ( caseClass.companionGen.jsonFormat )
s"""
${jsonReadsWritesBody.trim}

lazy val jsonFormat = JsonAssist.utils.lazyFormat(Format(jsonReads, jsonWrites))
"""
    else
      ""
}
object lenses {
${lensesBody.indent("  ")}
}

object parameters {
${parametersBody.indent("  ")}
}
${rpcHandlerBody}
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

  }
