package a8.codegen

import java.io.File

object ProjectRoot {
  def apply(dir: String): ProjectRoot =
    apply(new java.io.File(dir))

  def apply(dir: File): ProjectRoot =
    new ProjectRoot(dir.getCanonicalFile)
}

case class ProjectRoot private (dir: File) {

}
