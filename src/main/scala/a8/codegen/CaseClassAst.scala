package a8.codegen

object CaseClassAst {

  case class PackageName(value: String) {
    override def toString: String = value
  }
  case class CaseClassName(value: String) {
    override def toString: String = value
  }

  case class SourceFile(
    pakkage: PackageName,
    caseClasses: Iterable[CaseClass]
  )

  case class CaseClass(
    file: java.io.File,
    name: CaseClassName,
    properties: Iterable[Property],
    companionGen: CompanionGen,
    annotations: Iterable[Annotation],
  ) {

    lazy val qualifiedName = file.getCanonicalPath

    lazy val primaryKey: Option[Property] =
      properties
        .find(_.annotations.exists(_.name == "PK"))

  }

  case class Annotation(
    name: String,
    parms: Iterable[AnnotationParm] = Iterable.empty
  ) {
    def append(parm: AnnotationParm): Annotation =
      copy(parms = parms ++ Some(parm))
  }

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
    val nameAsVal: String =
      rawName match {
        case "type" =>
          "`type`"
        case s =>
          s
      }
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

  object TypeName {

    def apply(rawTypeName: String): TypeName =
      parse(rawTypeName)

    def parse(rawTypeName: String): TypeName = {
      TypeNameParser.parser.parseAll(rawTypeName) match {
        case Left(error) =>
          sys.error(s"error parsing type name ${rawTypeName} -- ${error}")
        case Right(tn) =>
          tn
      }
    }

    case class TupleTypeName(args: Iterable[TypeName]) extends TypeName {
      override def isOption: Boolean = false
      override def fullName: String =
        args.mkString("(", ",", ")")
    }

    case class RegularTypeName(name: String, args: Iterable[TypeName]) extends TypeName {
      override def isOption: Boolean = name == "Option"
      override def fullName: String = {
        args.isEmpty match {
          case true =>
            name
          case false =>
            name + args.mkString("[", ",", "]")
        }
      }
    }

  }

  sealed trait TypeName {
    def args: Iterable[TypeName]
    def fullName: String
    def isOption: Boolean
    override def toString: String = fullName
  }


}
