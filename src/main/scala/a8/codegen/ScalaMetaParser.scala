package a8.codegen

import java.io.File

import a8.codegen.ProjectConfig.Anno

import scala.meta.{Defn, Source, Template, Term, Tree}
import MoreOps._
import a8.codegen.CaseClassAst.{Annotation, AnnotationParm, CaseClass, CaseClassName, PackageName}

import scala.meta._

object ScalaMetaParser {

  def parseSourceFile(file: File, resolveCompanionGen: (CaseClassName, Anno)=>CompanionGen): CaseClassAst.SourceFile = {

    val source: Source = file.readText.parse[Source].get

    val allCaseClassDefs = visit(source)

    toString

    val caseClasses: Iterable[CaseClass] =
      allCaseClassDefs
        .filter(generateCodeFor)
        .map(caseClassDef)
        .map { ccd =>
          CaseClassAst.CaseClass(
            file,
            ccd.name,
            ccd.parms.map { parm =>
              CaseClassAst.Property(
                parm.name,
                CaseClassAst.TypeName(parm.tipe),
                parm.default,
                parm.annotations,
              )
            },
            resolveCompanionGen(ccd.name, ccd.companionGen),
            ccd.annotations
          )
        }

    CaseClassAst.SourceFile(
      pakkage(source),
      caseClasses,
    )

  }

  case class CaseClassDef(
    companionGen: Anno,
    name: CaseClassName,
    parms: Iterable[ParmDef],
    annotations: Iterable[Annotation],
  )

  case class ParmDef(
    name: String,
    tipe: String,
    default: Option[String],
    annotations: Iterable[Annotation],
  )

  def warn(msg: String) =
    println(s"warning -- ${msg}")

  def visit(s: Source): Vector[Defn.Class] = {
    s.stats
      .toVector
      .flatMap(stat =>
        visitTree(stat)
      )
  }

  def pakkage(source: Source): CaseClassAst.PackageName = {
    source
      .stats
      .flatMap {
        case p: Pkg =>
          Some(PackageName(p.ref.syntax))
        case _ =>
          None
      }
      .headOption
      .getOrElse(PackageName(""))
  }

  def visitTree(tree: Tree): Vector[Defn.Class] = {
    tree match {
      case pkg: Pkg =>
        pkg.stats.flatMap(visitTree).toVector
      case dc: Defn.Class =>
        Vector(dc)
      case o@ Defn.Object(mods, name, template) =>
        visitTemplate(template)
      case _ =>
        Vector.empty
    }
  }

  def visitTemplate(template: Template): Vector[Defn.Class] = {
    template.stats.flatMap(visitTree).toVector
  }

  def isCaseClass(mods: List[scala.meta.Mod]): Boolean =
    mods.exists {
      case _: scala.meta.Mod.Case =>
        true
      case _ =>
        false
    }

  def generateCodeFor(classDef: Defn.Class): Boolean =
    isCaseClass(classDef.mods) && hasCompanionGen(classDef.mods)

  def hasCompanionGen(mods: List[scala.meta.Mod]): Boolean =
    mods.exists {
      case anno: scala.meta.Mod.Annot if anno.init.tpe.syntax == "CompanionGen" =>
        true
      case _ =>
        false
    }

  def annotations(mods: List[Mod]): Iterable[Annotation] = {
    mods.collect {
      case anno: Mod.Annot =>
        annotation(anno)
    }
  }

  def annotation(annot: Mod.Annot): Annotation = {
    annot
      .init
      .argss
      .flatten
      .foldLeft(Annotation(annot.init.tpe.syntax)) { case (anno, term) =>
        term match {
          case Term.Assign(lhs, rhs) =>
            anno.append(
              AnnotationParm(
                lhs.syntax.trim,
                rhs.syntax.trim,
              )
            )
          case t =>
            warn(s"ignoring term in anno ${t.syntax}")
            anno
        }
      }
  }

  def caseClassDef(classDef: Defn.Class): CaseClassDef = {
    CaseClassDef(
      companionGenAnno(classDef),
      CaseClassName(classDef.name.value),
      classDef.ctor.paramss.flatten.map(parmDef),
      annotations(classDef.mods),
    )
  }

  def parmDef(param: scala.meta.Term.Param): ParmDef = {
    ParmDef(
      param.name.value,
      param.decltpe.map(_.syntax.trim).getOrElse(sys.error("case class parms must have types")),
      param.default.map(_.syntax.trim),
      annotations(param.mods),
    )

  }

  def companionGenAnno(classDef: Defn.Class): Anno = {
    classDef
      .mods
      .flatMap {
        case anno: scala.meta.Mod.Annot if anno.init.tpe.syntax == "CompanionGen" =>
          Some(companionGenAnno(anno))
        case _ =>
          None
      }
      .headOption
      .getOrElse(
        Anno()
      )
  }

  def companionGenAnno(mod: scala.meta.Mod.Annot): Anno = {
    mod
      .init
      .argss
      .flatten
      .foldLeft(Anno()) { case (anno, term) =>
        term match {
          case Term.Assign(lhs, rhs) =>
            val value =
              rhs.syntax.trim.toLowerCase match {
                case "true" =>
                  Some(true)
                case "false" =>
                  Some(false)
                case _ =>
                  warn(s"unable to parse ${rhs.syntax} to a boolean")
                  None
              }
            value
              .map(v => anno.append(lhs.syntax.trim -> v))
              .getOrElse(anno)
          case t =>
            warn(s"ignoring term ${t.syntax}")
            anno
        }
      }
  }

}
