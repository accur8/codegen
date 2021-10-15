package a8.codegen


import a8.codegen.CompanionGen.CompanionGenResolver

import java.io.File
import MoreOps._

import java.nio.file.Path

case class Project(
  root: ProjectRoot,
) {

  lazy val companionGenResolver = CompanionGen.resolver(this)

  lazy val config: ProjectConfig =
    findCodegenDotJson(root.dir)
      .getOrElse(sys.error(s"unable to find codegen.json at ${root.dir.toFile.getAbsolutePath}"))

  def findCodegenDotJson(dir: Path): Option[ProjectConfig] = {
    val codegenDotJsonFile = new File(root.dir.toFile, "codegen.json")

    if ( codegenDotJsonFile.exists() ) {
      val jsonStr = codegenDotJsonFile.readText
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      decode[ProjectConfig](jsonStr) match {
        case Left(th) =>
          throw new RuntimeException(s"error parsing ${codegenDotJsonFile} -- ${th.getMessage}")
        case Right(v) =>
          println(s"loaded codegen config from ${codegenDotJsonFile.getCanonicalPath}")
          Some(v)
      }
    } else {
      Option(dir.getParent)
        .flatMap(findCodegenDotJson)
    }

  }
}
