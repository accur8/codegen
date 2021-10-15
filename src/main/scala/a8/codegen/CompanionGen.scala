package a8.codegen

import com.sun.tools.javac.file.RelativePath
import java.io.File
import java.nio.file.{FileSystem, FileSystems, Path, PathMatcher}

import MoreOps._
import a8.codegen.CaseClassAst.CaseClassName
import a8.codegen.ProjectConfig.Anno

object CompanionGen {

//  case class CompanionGen(writeNones: Boolean = false, jsonFormat: Boolean = true, rpcHandler: Boolean = false, rowReader: Boolean = false, messagePack: Boolean = true)

  def resolver(project: Project): CompanionGenResolver = {
    CompanionGenResolver(project.root, project.config)
  }


  case class CompanionGenResolver(codeRoot: ProjectRoot, projectConfig: ProjectConfig) {

    lazy val canonicalCodeRoot = codeRoot.dir.toFile.getCanonicalFile.toPath

    def resolve(caseClassName: CaseClassName, file: java.io.File, annotations: Anno, defaultCompanionGen: CompanionGen): CompanionGen = {
      val resolvedAnno =
        projectConfig
          .defaults
          .foldLeft(Anno()) { (anno, default) =>
            val absolutePath = file.getCanonicalFile.toPath
            val relativePath = canonicalCodeRoot.relativize(absolutePath)
            if ( default.matches(relativePath) ) {
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
        jdbcMapper = resolvedAnno.bool("jdbcMapper").getOrElse(defaultCompanionGen.jdbcMapper),
        messagePack = resolvedAnno.bool("messagePack").getOrElse(defaultCompanionGen.messagePack),
        qubesMapper = resolvedAnno.bool("qubesMapper").getOrElse(defaultCompanionGen.qubesMapper),
        circeCodec = resolvedAnno.bool("circeCodec").getOrElse(defaultCompanionGen.circeCodec),
        jsonCodec = resolvedAnno.bool("jsonCodec").getOrElse(defaultCompanionGen.jsonCodec),
      )
    }

  }

  val empty =
    CompanionGen(
      writeNones = false,
      jsonFormat = false,
      rpcHandler = false,
      jdbcMapper = false,
      messagePack = false,
      qubesMapper = false,
      circeCodec = false,
      jsonCodec = false,
    )

}



/**
 * CompanionGen annos exist in a8.common (model3) and a8.codegen (sync/shared)
 */
case class CompanionGen(
  writeNones: Boolean,
  jsonFormat: Boolean,
  rpcHandler: Boolean,
  jdbcMapper: Boolean,
  messagePack: Boolean,
  qubesMapper: Boolean,
  circeCodec: Boolean,
  jsonCodec: Boolean,
)

