package a8.codegen

import com.sun.tools.javac.file.RelativePath

object CompanionGen {

2


  case class Default(
    filter: String,
    annotations: Annotations,
  ) {
    def matches(caseClassName: String, relativeFilePath: RelativePath): Boolean = ???
  }

  case class Annotations(
    writeNones: Option[Boolean] = None,
    jsonFormat: Option[Boolean] = None,
    rpcHandler: Option[Boolean] = None,
    rowReader: Option[Boolean] = None,
    messagePack: Option[Boolean] = None,
  ) {

    def merge(right: Annotations) =
      Annotations(
        right.writeNones.orElse(writeNones),
        right.jsonFormat.orElse(jsonFormat),
        right.rpcHandler.orElse(rpcHandler),
        right.rowReader.orElse(rowReader),
        right.messagePack.orElse(messagePack),
      )

  }

//  case class CompanionGen(writeNones: Boolean = false, jsonFormat: Boolean = true, rpcHandler: Boolean = false, rowReader: Boolean = false, messagePack: Boolean = true)

  def resolver(root: java.io.File): CompanionGenResolver = {
    ???
  }


  case class CompanionGenResolver(root: java.io.File, defaults: Iterable[Default]) {

    def resolve(caseClassName: String, file: java.io.File, annotations: Annotations): CompanionGen = {
      val resolvedAnno =
        defaults
          .foldLeft(Annotations()) { (anno, default) =>
            if ( default.matches(caseClassName, file) ) {
              anno.merge(default.annotations)
            } else {
              anno
            }
          }
          .merge(annotations)
      CompanionGen(
        writeNones = resolvedAnno.writeNones.getOrElse(false),
        jsonFormat = resolvedAnno.jsonFormat.getOrElse(true),
        rpcHandler = resolvedAnno.rpcHandler.getOrElse(false),
        rowReader = resolvedAnno.rowReader.getOrElse(false),
        messagePack = resolvedAnno.messagePack.getOrElse(false),
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

