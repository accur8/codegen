package a8.codegen


import a8.codegen.CompanionGen.CompanionGenResolver

import java.io.File
import MoreOps._

import java.nio.file.Path

case class Project(
  root: ProjectRoot,
  searchRoot: java.io.File,
  configFile: java.io.File,
  config: ProjectConfig,
  scala3: Boolean,
) {

  lazy val companionGenResolver = CompanionGen.resolver(this)

}
