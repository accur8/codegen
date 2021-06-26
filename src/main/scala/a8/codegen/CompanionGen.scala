package a8.codegen

import com.sun.tools.javac.file.RelativePath

import java.io.File
import java.nio.file.{FileSystem, FileSystems, Path, PathMatcher}
import MoreOps._
import a8.codegen.ProjectConfig.Anno

object CompanionGen {

//  case class CompanionGen(writeNones: Boolean = false, jsonFormat: Boolean = true, rpcHandler: Boolean = false, rowReader: Boolean = false, messagePack: Boolean = true)

  def resolver(project: Project): CompanionGenResolver = {
    CompanionGenResolver(project.root, project.config)
  }


  case class CompanionGenResolver(codeRoot: ProjectRoot, projectConfig: ProjectConfig) {

    def resolve(caseClassName: String, file: java.io.File, annotations: Anno, defaultCompanionGen: CompanionGen): CompanionGen = {
      val resolvedAnno =
        projectConfig
          .defaults
          .foldLeft(Anno()) { (anno, default) =>
            if ( default.matches(file.toPath) ) {
              anno.merge(default.resolvedAnno)
            } else {
              anno
            }
          }
          .merge(annotations)
      CompanionGen(
        writeNones = resolvedAnno.bool("writeNones").getOrElse(defaultCompanionGen.writeNones),
        jsonFormat = resolvedAnno.bool("jsonFormat").getOrElse(defaultCompanionGen.jsonFormat),
        rpcHandler = resolvedAnno.bool("rpcHandler").getOrElse(defaultCompanionGen.rpcHandler),
        rowReader = resolvedAnno.bool("rowReader").getOrElse(defaultCompanionGen.rowReader),
        messagePack = resolvedAnno.bool("messagePack").getOrElse(defaultCompanionGen.messagePack),
      )
    }

  }

}



/**
 * CompanionGen annos exist in a8.common (model3) and a8.codegen (sync/shared)
 */
case class CompanionGen(
  writeNones: Boolean,
  jsonFormat: Boolean,
  rpcHandler: Boolean,
  rowReader: Boolean,
  messagePack: Boolean,
)

