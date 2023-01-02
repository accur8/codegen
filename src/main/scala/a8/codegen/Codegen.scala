package a8.codegen

import a8.codegen.CaseClassAst.CaseClass

import java.io.{File, StringWriter}
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver
import a8.codegen.MoreOps.FileOps
import cats.effect.{ExitCode, IO, IOApp}

import java.nio.file.{FileVisitOption, Path}
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
          Some(Project(ProjectRoot(dir), dir, codegenDotJsonFile, cfg))
      }
    } else if ( parent != null && searchParent ) {
      loadCodegenJson(parent, searchParent)
    } else {
      None
    }
  }

  def findProjectRoots(dir: File): fs2.Stream[IO,Project] = {
    val stream: fs2.Stream[IO, Option[Project]] =
      loadCodegenJson(dir, true) match {
        case Some(project) =>
          fs2.Stream(Some(project.copy(searchRoot = dir))).covary[IO]
        case None =>
          fs2.io.file
            .Files[IO]
            .walk(dir.toPath, Seq(FileVisitOption.FOLLOW_LINKS))
            //      .evalMap { f =>
            //        IO.blocking {
            //          println(f)
            //          f
            //        }
            //      }
            .parEvalMapUnordered(15) {
              case p if p.toFile.getName == "codegen.json" =>
                IO(loadCodegenJson(p.getParent.toFile, false))
              case _ =>
                IO.none
            }
      }
    stream.flatMap(o => fs2.Stream.iterable(o))
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
           |Usage: a8-codegen [ --help | --l-help | template1 | template2 ]
           |
           |Finds scala files in current directory with @CompanionGen and generates companion case classes
           |
           |  templateName
           |
           |  --help      shows help for a8-codegen
           |
           |  --l-help    shows the options for the app launcher (like how to update the app)
         """.stripMargin
      println(help)
      if ( args.nonEmpty )
        ExitCode.Error
      else
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

