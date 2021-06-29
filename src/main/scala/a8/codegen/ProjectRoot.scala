package a8.codegen

import java.io.File
import java.nio.file.Path

object ProjectRoot {
  def apply(dir: String): ProjectRoot =
    apply(new java.io.File(dir))

  def apply(dir: File): ProjectRoot =
    new ProjectRoot(dir.getCanonicalFile.toPath)
}

case class ProjectRoot private (dir: Path) {

}
