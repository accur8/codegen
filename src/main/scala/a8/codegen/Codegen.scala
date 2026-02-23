package a8.codegen

import a8.codegen.CaseClassAst.CaseClass

import java.io.{File, StringWriter}
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver
import a8.codegen.MoreOps.FileOps
import cats.effect.{ExitCode, IO, IOApp}

import java.nio.file.{FileSystems, Path, PathMatcher}
import scala.language.postfixOps
import scala.util.Try
import scala.util.control.NonFatal
import cats.implicits._
import cats.effect.implicits._

object Codegen extends IOApp {

  val maxConcurrent = 20

  sealed trait CodegenAction
  case class RunCodegen(file: File) extends CodegenAction
  case class CodeGenerated(file: File) extends CodegenAction with CodeGenResult

  sealed trait CodeGenResult
  case class CodeGenSuccess(sourceFile: File, generatedFile: File) extends CodeGenResult
  case class CodeGenFailure(sourceFile: Option[File], failure: Throwable) extends CodeGenResult

  def codegenAction(p: Path): Option[CodegenAction] = {
    val f = p.toFile
    val name = f.getName
    if ( f.isFile && name.endsWith(".scala") ) {
      if ( name.startsWith("Mx") ) {
        Some(CodeGenerated(f))
      } else {
        loadFileContents(f)
          .exists { contents =>
            contents.contains("@CompanionGen") && !contents.contains("//@NoCodegen")
          }
          .option(RunCodegen(f))
      }
    } else {
      None
    }
  }

  def isMxScalaFile(p: Path): Boolean = {
    val f = p.toFile
    val name = f.getName
    if (
      f.isFile
        && name.endsWith(".scala")
        && name.startsWith("Mx")
    ) {
      loadFileContents(f)
        .exists { contents =>
          contents.contains("@CompanionGen") && !contents.contains("//@NoCodegen")
        }
    } else {
      false
    }
  }

  def loadCodegenJson(dir: File, searchParent: Boolean): Option[Project] = {
    val codegenDotJsonFile = new File(dir, "codegen.json")
    val s = dir.getCanonicalPath
    val parent = dir.getParentFile
    if ( codegenDotJsonFile.exists() ) {
      val jsonStr = codegenDotJsonFile.readText
      import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
      decode[ProjectConfig](jsonStr) match {
        case Left(th) =>
          throw new RuntimeException(s"error parsing ${codegenDotJsonFile} -- ${th.getMessage}")
        case Right(cfg) =>
          val scala3 =
            cfg
              .defaults
              .flatMap(_.anno.get("scala3"))
              .filter(b => b)
              .headOption
              .nonEmpty
          this.toString
          Some(Project(ProjectRoot(dir), dir, codegenDotJsonFile, cfg, scala3))
      }
    } else if ( parent != null && searchParent ) {
      loadCodegenJson(parent, searchParent)
    } else {
      None
    }
  }

  // Parse .gitignore in dir and return PathMatchers for each pattern
  def readGitignorePatterns(dir: File): Seq[PathMatcher] = {
    val gitignoreFile = new File(dir, ".gitignore")
    if (!gitignoreFile.exists()) return Seq.empty
    val fs = FileSystems.getDefault()
    val lines =
      try {
        val source = scala.io.Source.fromFile(gitignoreFile)
        try source.getLines().toList
        finally source.close()
      } catch {
        case _: Exception => List.empty
      }
    lines
      .map(_.trim)
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .flatMap { pattern =>
        // Strip trailing slashes for directory patterns — we match the name
        val p = pattern.stripSuffix("/")
        // If pattern contains no slash (or only a leading slash stripped already),
        // match against the filename alone; otherwise match against full path glob
        val globPatterns =
          if (p.contains("/")) {
            // Path-relative pattern: match against path relative to dir
            List(s"glob:$p")
          } else {
            // Filename-only pattern: match the name anywhere in the tree
            List(s"glob:$p", s"glob:**/$p")
          }
        globPatterns.flatMap { g =>
          try List(fs.getPathMatcher(g))
          catch { case _: Exception => List.empty }
        }
      }
  }

  def isIgnoredByPatterns(file: File, patterns: Seq[PathMatcher]): Boolean = {
    if (patterns.isEmpty) return false
    val namePath = java.nio.file.Paths.get(file.getName)
    val absPath  = file.toPath
    patterns.exists { pm =>
      try pm.matches(namePath) || pm.matches(absPath)
      catch { case _: Exception => false }
    }
  }

  // Custom recursive walk: at the search root we look inside all non-ignored subdirs;
  // below the search root we only descend into subdirs that have a codegen.json.
  def walkForProjectRoots(dir: File, parentPatterns: Seq[PathMatcher], isSearchRoot: Boolean): fs2.Stream[IO, Project] = {
    val localPatterns = parentPatterns ++ readGitignorePatterns(dir)
    val children = Option(dir.listFiles()).map(_.toList).getOrElse(List.empty)
    val filtered = children.filter { f =>
      val name = f.getName
      name != ".git" && !isIgnoredByPatterns(f, localPatterns)
    }
    fs2.Stream.emits(filtered).covary[IO].flatMap { f =>
      if (f.isDirectory) {
        val hasCodegenJson = new File(f, "codegen.json").exists()
        if (hasCodegenJson) {
          val project = fs2.Stream.fromOption(loadCodegenJson(f, false)).covary[IO]
          project ++ walkForProjectRoots(f, localPatterns, isSearchRoot = false)
        } else if (isSearchRoot) {
          // At the search root we still recurse into dirs without codegen.json
          walkForProjectRoots(f, localPatterns, isSearchRoot = false)
        } else {
          fs2.Stream.empty
        }
      } else {
        fs2.Stream.empty
      }
    }
  }

  def findProjectRoots(dir: File): fs2.Stream[IO,Project] = {
    loadCodegenJson(dir, true) match {
      case Some(project) =>
        fs2.Stream(project.copy(searchRoot = dir)).covary[IO]
      case None =>
        walkForProjectRoots(dir, parentPatterns = Seq.empty, isSearchRoot = true)
    }
  }

  def findCodegenScalaFiles(dir: File): fs2.Stream[IO,CodegenAction] =
    fs2.io.file
      .Files[IO]
      .walk(dir.toPath)
      .parEvalMapUnordered(maxConcurrent) { p =>
        IO.blocking {
          codegenAction(p)
        }
      }
      .collect {
        case Some(ca) =>
          ca
      }

  def printHelp(args: Option[List[String]] = None): IO[ExitCode] = {
    IO.blocking {
      args.map{ a =>
        println(s"a8-codegen does not support args = ${a.toList}")
        println("")
      }

      val help =
        s"""Accur8 Codegen Tool
           |
           |Usage: a8-codegen [ --help | --l-help | init [directory] | [directory...] ]
           |
           |Finds scala files with @CompanionGen and generates companion case classes
           |
           |  init [directory]  generate a default codegen.json in directory (defaults to current directory)
           |
           |  directory         one or more directories to process (defaults to current directory)
           |
           |  --help            shows help for a8-codegen
           |
           |  --l-help          shows the options for the app launcher (like how to update the app)
         """.stripMargin
      println(help)
      if ( args.nonEmpty )
        ExitCode.Error
      else
        ExitCode.Success
    }
  }

  val defaultCodegenJson: String =
    """|{
       |  "template": "template2",
       |  "defaults": [
       |    {
       |      "anno": {
       |        "writeNones": false,
       |        "jsonFormat": false,
       |        "rpcHandler": false,
       |        "jdbcMapper": false,
       |        "queryDsl": false,
       |        "messagePack": false,
       |        "qubesMapper": false,
       |        "circeCodec": false,
       |        "jsonCodec": true,
       |        "zio": false,
       |        "scala3": false,
       |        "cats": false
       |      }
       |    }
       |  ]
       |}
       |""".stripMargin

  def initCodegenJson(dir: File): IO[ExitCode] = IO.blocking {
    val target = new File(dir, "codegen.json")
    if (target.exists()) {
      println(s"codegen.json already exists at ${target.getCanonicalPath} -- not overwriting")
      ExitCode.Error
    } else {
      if (!dir.exists()) dir.mkdirs()
      val pw = new java.io.PrintWriter(target)
      try pw.write(defaultCodegenJson)
      finally pw.close()
      println(s"created ${target.getCanonicalPath}")
      ExitCode.Success
    }
  }


  override def run(args: List[String]): IO[ExitCode] = {
    import sys.process._

    args match {
      case List() =>
        runCodeGen(new File("."))
      case List("--help") =>
        printHelp()
      case List("--l-help") =>
        IO.blocking(
          "a8-codegen --l-help"!
        ).as(ExitCode.Success)
      case List("init") =>
        initCodegenJson(new File("."))
      case List("init", dir) =>
        initCodegenJson(new File(dir))
      case dirs if dirs.forall(!_.startsWith("--")) =>
        // Run codegen on each specified directory
        dirs
          .map(new File(_))
          .traverse_(runCodeGen)
          .as(ExitCode.Success)
      case _ =>
        printHelp(Some(args))
    }

  }


  def runCodeGen(dir: File): IO[ExitCode] =
    findProjectRoots(dir)
      .parEvalMapUnordered(maxConcurrent) (pr =>
        IO.delay(codeGenScalaFiles(pr))
      )
      .parJoin(maxConcurrent)
      .evalMap { result =>
        IO.blocking {
          result match {
            case CodeGenSuccess(sourceFile, generatedFile) =>
              println(s"generated ${generatedFile.getCanonicalPath}")
            case _ =>
          }
          result
        }
      }
      .compile
      .toList
      .flatMap { results =>

        val generatedFiles =
          results
            .collect {
              case cgs: Codegen.CodeGenSuccess =>
                cgs.generatedFile
            }
            .toSet

        val beforeGeneratedFiles =
          results
            .collect {
              case CodeGenerated(f) =>
                f
            }
            .toSet

        // cleanup previously generated files that were not generated this run
        beforeGeneratedFiles
          .filterNot(generatedFiles.contains)
          .foreach { obsoleteFile =>
            println(s"cleaning up obsolete file ${obsoleteFile.getAbsolutePath}")
            obsoleteFile.delete()
          }

        val failures =
          results
            .collect {
              case cgf: CodeGenFailure =>
                cgf
            }
        if ( failures.isEmpty ) {
          IO.blocking {
            println(s"SUCCESS")
          }.as(ExitCode.Success)
        } else {
          IO.blocking {
            failures.foreach { failure =>
              println(s"failure generating ${failure.sourceFile.map(_.getAbsolutePath)}")
              failure.failure.printStackTrace()
              println(s"FAILURE")
            }
          }.as(ExitCode.Success)
        }
      }


  def codeGenScalaFiles(project: Project): fs2.Stream[IO,CodeGenResult] = {

    val templateFactory = CodegenTemplate(project.config.template)

    println(s"processing scala files with @CompanionGen in ${project.searchRoot.getCanonicalPath} using config ${project.configFile.getCanonicalPath}")

    findCodegenScalaFiles(project.searchRoot)
      .parEvalMapUnordered(maxConcurrent) {
        case RunCodegen(f) =>
          templateFactory(f, project).run()
        case cg: CodeGenerated =>
          IO(cg)
      }

  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) = {
    // take care to generate all the code and then write the new file so we don't half write the file and then crash
    val sw = new StringWriter()
    val p = new java.io.PrintWriter(sw)
    try {
      op(p)
      val actual = new java.io.PrintWriter(f)
      try {
        actual.write(sw.toString)
      } finally {
        actual.close()
      }
    } finally {
      p.close()
    }
  }

  def loadFileContents(f: File): Option[String] = {
    val source = scala.io.Source.fromFile(f)
    try {
      Some(source.getLines().mkString("\n"))
    } catch {
      case e: Exception =>
        println(s"error processing ${f}")
        e.printStackTrace()
        None
    } finally {
      source.close()
    }
  }

}

