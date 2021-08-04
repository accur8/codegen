package a8.codegen


import a8.codegen.CaseClassAst.CaseClass
import a8.codegen.CodegenTemplate.TemplateFactory
import a8.codegen.CommonOpsCopy._
import a8.codegen.FastParseTools.Source
import cats.effect.{IO, IOApp}

import java.io.File
import scala.language.postfixOps

object CodegenTemplate2 extends TemplateFactory with IOApp.Simple {


  override def run: IO[Unit] = {
//    Codegen.runCodeGen(new File("c:/Users/glen/code/accur8/composite"))
    Codegen
      .runCodeGen(new File("/Users/glen/code/accur8/composite"))
      .void
//    Codegen.codeGenScalaFiles(ProjectRoot("/Users/glen/code/accur8/composite/sync"))
//    Codegen.codeGenScalaFiles(ProjectRoot("/Users/glen/code/accur8/composite/wsjdbc"))
  }

}


case class CodegenTemplate2(file: java.io.File, project: Project) extends CodegenTemplate {

  override val companionGenDefault: CompanionGen = CompanionGen.empty

  val manualImports =
    previousGeneratedSourceCode
      .linesIterator
      .toList
      .dropWhile(!_.startsWith("//===="))
      .drop(1)
      .takeWhile(!_.startsWith("//===="))

  lazy val previousGeneratedSourceCode =
    if ( generatedFile.exists() ) {
      val s = scala.io.Source.fromFile(generatedFile)
      val gf = s.getLines.mkString("\n")
      s.close()
      gf
    } else {
      ""
    }

  lazy val generatedFile = new java.io.File(file.getParentFile, "Mx" + file.getName)


//  val sourceCode = scala.io.Source.fromFile("model/shared/src/main/scala/a8/manna/model/Tester2.scala").mkString
  lazy val sourceCode = {
    val s = scala.io.Source.fromFile(file)
    val sc = s.getLines.mkString("\n")
    s.close()
    sc
  }

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

  case class CaseClassGen(cc: CaseClass) {

    lazy val props: Iterable[CaseClassAst.Property] = cc.properties

    lazy val parametersBody: String =
      props
        .zipWithIndex
        .map { case (prop, ordinal) =>
          s"lazy val ${prop.nameAsVal}: CaseClassParm[${cc.name},${prop.typeName}] = CaseClassParm[${cc.name},${prop.typeName}](${prop.nameAsStringLit}, _.${prop.nameAsVal}, (d,v) => d.copy(${prop.nameAsVal} = v), ${prop.defaultExpr.map("()=> " + _)}, ${ordinal})"
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

    lazy val bareBody = s"""

${
      BuilderTemplate
        .templates
        .flatMap(_.build(cc))
        .mkString("\n\n")
}

implicit val catsEq: cats.Eq[${cc.name}] = cats.Eq.fromUniversalEquals

lazy val generator: Generator[${cc.name},parameters.type] =  {
  val constructors = Constructors[${cc.name}](${props.size}, unsafe.iterRawConstruct)
  Generator(constructors, parameters)
}

object parameters {
${parametersBody.indent("  ")}
}

${unsafeBody}

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
