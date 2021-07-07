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
    file: java.io.File,
    name: String,
    properties: Iterable[Property],
    companionGen: CompanionGen,
    annotations: Iterable[Annotation],
  ) {
    val qualifiedName = file.getCanonicalPath
  }

  case class Annotation(
    name: String,
    parms: Iterable[AnnotationParm]
  )

  case class AnnotationParm(
    name: String,
    value: String,
  )

  case class Property(
    rawName: String,
    typeName: TypeName,
    defaultExpr: Option[String],
    annotations: Iterable[Annotation],
  ) {
    val nameAsVal: String = rawName
    val nameAsStringLit: String = {
      val s =
        if ( rawName.startsWith("`") )
          rawName.substring(1, rawName.length-1)
        else
          rawName
      '"'.toString + s + '"'.toString
    }

    override def toString =
      nameAsVal + ": " + typeName + defaultExpr.map(" = " + _).getOrElse("")
  }

  case class TypeName(name: String, args: Iterable[TypeName] = Nil) {
    def isOption = name == "Option"
    override def toString =
      name + (if ( args.nonEmpty ) ("[" + args.map(_.toString).mkString(",") + "]") else "")
  }

}
