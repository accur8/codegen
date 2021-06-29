package a8.codegen


import a8.codegen.CompanionGen.CompanionGenResolver

import java.io.File
import MoreOps._

case class Project(
  root: ProjectRoot,
) {

  lazy val companionGenResolver = CompanionGen.resolver(this)

  lazy val config: ProjectConfig = {

    val codegenDotJsonFile = new File(root.dir.toFile, "codegen.json")

    if ( !codegenDotJsonFile.exists() )
      sys.error(s"missing required file codegen.json ${codegenDotJsonFile}")

    val jsonStr = codegenDotJsonFile.readText
    import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
    decode[ProjectConfig](jsonStr) match {
      case Left(th) =>
        throw new RuntimeException(s"error parsing ${codegenDotJsonFile} -- ${th.getMessage}")
      case Right(v) =>
        v
    }

  }

}
