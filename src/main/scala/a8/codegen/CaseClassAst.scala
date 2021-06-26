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

  case class
  ASFSAFSADFCaseClass(
    name: String,
    properties: Iterable[Property],
    companionGen: CompanionGen,
  )

  case class Annotation(
    name: String,
    parms: Iterable[AnnotationParm]
  )

  case class AnnotationParm(
    name: String,
    value: String,
  )

  case class Property(
    name: String,
    typeName: TypeName,
    defaultExpr: Option[String],
    annotations: Iterable[Annotation],
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
