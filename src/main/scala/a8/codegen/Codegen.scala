package a8.codegen

import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.FastParseTools.{ParserConfig, Source}
import java.io.File
import CommonOpsCopy._

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
      contents.contains("@CompanionGen")
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

  def main(args: Array[String]) = {

    println("finding scala files with @CompanionGen")
    val files = findCodegenScalaFiles(new File("."))
    println(s"found ${files.size} scala files")

    files
      .foreach( file =>
        Codegen(file)
          .run()
      )

  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def loadFileContents(f: File): String = {
    val source = scala.io.Source.fromFile(f)
    try source.getLines.mkString("\n") finally source.close()
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
import play.api.libs.json.{JsPath, Reads, Writes}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import a8.common.CommonOps._


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
case class CompanionGen(writeNones: Boolean = false)

case class CaseClassGen(caseClass: CaseClass) {

  import Codegen._

  lazy val cc = caseClass
  lazy val props = caseClass.properties

  lazy val jsonFieldReads =
    props
      .map { prop =>
        val (verb, typeName) =
          if ( prop.typeName.isOption ) "readNullable" -> prop.typeName.args.head
          else "read" -> prop.typeName
        val readCall =
          prop
            .defaultExpr
            .map(defaultVal => s"x${verb}WithDefault[${typeName}](${defaultVal})")
            .getOrElse(s"${verb}[${typeName}]")
        s"""(JsPath \\ "${prop.name}").${readCall}"""
      }
      .toList
      .mkString(" and\n")

  lazy val jsonFieldWrites =
    props
      .map { prop =>
        val (verb, typeName) =
          if ( prop.typeName.isOption && !caseClass.companionGen.writeNones ) "writeNullable" -> prop.typeName.args.head
          else "write" -> prop.typeName
        s"""(JsPath \\ "${prop.name}").${verb}[${typeName}]"""
      }
      .toList
      .mkString(" and\n")

  lazy val jsonReadsWritesBody =
    props.size match {
      case 0 =>
        s"""
implicit val jsonReads: Reads[${cc.name}] = json.emptyObjectReader(${cc.name}())
implicit val jsonWrites: Writes[${cc.name}] = json.emptyObjectWriter[${cc.name}]
        """

      case 1 =>
        s"""
implicit val jsonReads: Reads[${cc.name}] =
  ${jsonFieldReads}.map(${cc.name}.apply)

implicit val jsonWrites: Writes[${cc.name}] =
  ${jsonFieldWrites}.contramap { v => v.${props.head.name} }
        """

      case _ =>
        s"""
implicit val jsonReads: Reads[${cc.name}] = (
${jsonFieldReads.indent("  ")}
)(${cc.name}.apply _)

implicit val jsonWrites: Writes[${cc.name}] = (
${jsonFieldWrites.indent("  ")}
)(unlift(${cc.name}.unapply))
        """
    }

  lazy val lensesBody =
    props
      .map { prop =>
        s"val ${prop.name}: Lens[${cc.name},${prop.typeName}] = LensImpl[${cc.name},${prop.typeName}](${prop.name.quoted}, _.${prop.name}, (d,v) => d.copy(${prop.name} = v))"
      }
      .mkString("\n")

  lazy val bareBody = s"""

${jsonReadsWritesBody.trim}

object lenses {
${lensesBody.indent("  ")}
}

val allLenses = List(${props.map(p => s"lenses.${p.name}").mkString(",")})

val allLensesHList = ${props.toNonEmpty.map(_.map(p => s"lenses.${p.name}").mkString("", " :: ", " :: ")).getOrElse("") + "shapeless.HNil"}

    """

  lazy val body = s"""
trait Mx${cc.name} {

${bareBody.trim.indent("  ")}

}
    """

  }
