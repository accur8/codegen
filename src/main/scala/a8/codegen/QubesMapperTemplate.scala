package a8.codegen

import a8.codegen.CaseClassAst.{CaseClass, TypeName}
import a8.codegen.CodegenTemplate2.ResolvedCaseClass
import CommonOpsCopy._

object
  QubesMapperTemplate
extends
  BuilderTemplate(
    "qubesMapper",
    TypeName("a8.sync.qubes.QubesMapper"),
    TypeName("a8.sync.qubes.QubesMapperBuilder"),
    generateFor = _.qubesMapper,
    callBuilderOverrideMethod = false,
  )
{

  override def resolveTypeClassName(caseClass: CaseClass): String = {
    caseClass.primaryKeys match {
      case Nil =>
        sys.error("@PK is required")
      case List(pk) =>
        s"a8.sync.qubes.QubesKeyedMapper[${caseClass.name},${pk.typeName}]"
      case l =>
        s"a8.sync.qubes.QubesKeyedMapper[${caseClass.name},(${l.map(_.typeName).mkString(",")})]"
    }
  }

  def primaryKeyLine(caseClass: CaseClass): String = {
    caseClass.primaryKeys match {
      case Nil =>
        sys.error(s"must supply PK on ${caseClass.name}")
      case List(pk) =>
        s".singlePrimaryKey(_.${pk.nameAsVal})"
      case l =>
        s".compositePrimaryKey${l.size}(s => (${l.map(k => s"s.${k.nameAsVal}").mkString(",")}))"
    }
    //          .getOrElse(sys.error(s"must supply PK on ${caseClass.name}"))
  }

  override def rawBuild(resolvedCaseClass: ResolvedCaseClass, includeBuildCall: Boolean = true): String = {
    import resolvedCaseClass.caseClass

    val qa = caseClass.qubesAnno

    val base = super.rawBuild(resolvedCaseClass, false)
    val suffix =
      List(
        s".cubeName(${qa.cube})",
        s".appSpace(${qa.appSpace})",
        s"${primaryKeyLine(caseClass)}",
        ".build",
      )

    (base + suffix.mkString("\n","\n","").indent("    ")).trim

  }

}
