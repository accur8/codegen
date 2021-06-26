package a8.codegen

import a8.codegen.ProjectConfig.AnnotationWithFilter

import java.nio.file.{FileSystems, Path, PathMatcher}


object ProjectConfig {

  /**
   * file filter uses getPathMatcher syntax as seen here https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#getPathMatcher(java.lang.String)
   *
   */
  case class AnnotationWithFilter(
    filter: Option[String],
    anno: Map[String,Boolean],
  ) {

    val resolvedAnno = Anno(anno)

    lazy val pathMatcher = {
      filter
        .map { ff =>
          try {
            FileSystems.getDefault().getPathMatcher(ff)
          } catch {
            case th: Throwable =>
              sys.error(s"invalid fileFilter ${filter} check https://docs.oracle.com/javase/7/docs/api/java/nio/file/FileSystem.html#getPathMatcher(java.lang.String) for how to form a valid one")
          }
        }
        .getOrElse(
          new PathMatcher {
            override def matches(path: Path): Boolean = true
          }
        )
    }

    def matches(relativeFilePath: Path): Boolean =
      pathMatcher.matches(relativeFilePath)

  }

  case class Anno(
    values: Map[String,Boolean] = Map.empty
  ) {

    def merge(right: Anno) = {
      Anno(this.values ++ right.values)
    }

    def bool(name: String): Option[Boolean] =
      values.get(name)

  }

}

case class ProjectConfig(
  template: String,
  defaults: Iterable[AnnotationWithFilter],
)
