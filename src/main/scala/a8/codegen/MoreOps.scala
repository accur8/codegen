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

  implicit class StringOps(string: String) {

    def indent(by: String): String =
      string
        .linesIterator
        .map(by + _)
        .mkString("\n")

    def stripQuotes: String =
      if ( string.length > 2 && string.head == '"' && string.last == '"' ) {
        string.substring(1, string.length-1)
      } else {
        string
      }

  }

}
