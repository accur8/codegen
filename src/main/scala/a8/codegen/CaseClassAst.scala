package a8.codegen

object CaseClassAst {


  case class SourceFile(
    pakkage: String,
    imports: Iterable[Import],
    caseClasses: Iterable[CaseClass]
  )

  case class Import(
    name: String
  )

  case class CaseClass(
    name: String,
    properties: Iterable[Property],
    companionGen: CompanionGen,
  )

  case class Property(
    name: String,
    typeName: TypeName,
    defaultExpr: Option[String],
  ) {
    override def toString =
      name + ": " + typeName + defaultExpr.map(" = " + _).getOrElse("")
  }

  case class TypeName(name: String, args: Iterable[TypeName] = Nil) {
    def isOption = name == "Option"
    override def toString =
      name + (if ( args.nonEmpty ) ("[" + args.map(_.toString).mkString(",") + "]") else "")
  }

}
