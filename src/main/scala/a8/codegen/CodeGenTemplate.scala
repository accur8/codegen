package a8.codegen


import java.io.File
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver
import a8.codegen.FastParseTools.ParserConfig

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


  lazy val parser = new CaseClassParser(file, resolveCompanionGen)(ParserConfig(true))

  def resolveCompanionGen(caseClassName: String, sourceAnno: a8.codegen.ProjectConfig.Anno): CompanionGen =
    project
      .companionGenResolver
      .resolve(caseClassName, file, sourceAnno, companionGenDefault)

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
