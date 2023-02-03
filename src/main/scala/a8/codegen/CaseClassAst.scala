package a8.codegen

import a8.codegen.BuilderTemplate.QubesAnno

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
  ) {
    lazy val caseClassesByName =
      caseClasses
        .map(cc => cc.name.value -> cc)
        .toMap
  }

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

    lazy val sqlTableAnno: Option[CaseClassAst.Annotation] =
      annotations
        .find(_.name == "SqlTable")

    lazy val sqlTableAnnoValue: Option[String] =
      sqlTableAnno
        .flatMap(_.parms.headOption.map(_.value))

    lazy val primaryKeys: List[Property] =
      properties
        .filter(_.annotations.exists(_.name == "PK"))
        .toList

    lazy val hasSqlTable = primaryKeys.nonEmpty || sqlTableAnno.nonEmpty

    lazy val qubesAnno: QubesAnno =
      annotations
        .find(_.name == "QubesAnno")
        .map { anno =>
          QubesAnno(
            cube = anno.parms.find(_.name == "cube").map(_.value).getOrElse('"'.toString + name + '"'.toString),
            appSpace = anno.parms.find(_.name == "appSpace").map(_.value).getOrElse(sys.error("""must supply @QubesAnno(appSpace = "foo") i.e. appSpace annotation field is required""")),
          )
        }
        .getOrElse(sys.error(s"""for qubesMapper minimally the @QubesAnno(appSpace = "foo") is required for every class marked with @CompanionGen() on ${qualifiedName}"""))

    lazy val primaryKeyTypeName = {
      primaryKeys match {
        case Nil =>
          None
        case List(e) =>
          Some(e.typeName.toString)
        case list =>
          Some(list.map(_.typeName.toString).mkString("(", ",", ")"))
      }
    }


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
