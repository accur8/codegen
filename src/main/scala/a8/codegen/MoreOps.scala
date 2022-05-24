package a8.codegen

object MoreOps {

  implicit class FileOps(file: java.io.File) {

    def readText: String =
      scala.io.Source.fromFile(file).getLines().mkString("\n")

    def readTextOpt: Option[String] = {
      if ( file.exists() )
        Some(readText)
      else
        None
    }

  }

  implicit class ZStringContextOps(val stringContext: StringContext) extends AnyVal {
    def z(args: String*): String =
      stringContext
        .parts
        .zip(args)
        .map(t => t._1 + t._2)
        .mkString + stringContext.parts.last
  }
}
