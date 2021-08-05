package a8.codegen

import a8.codegen.CaseClassAst.CaseClass

import java.io.{File, StringWriter}
import CommonOpsCopy._
import a8.codegen.CompanionGen.CompanionGenResolver
import cats.effect.{ExitCode, IO, IOApp}

import java.nio.file.{FileVisitOption, Path}
import scala.language.postfixOps
import scala.util.Try
import scala.util.control.NonFatal
import cats.implicits._
import cats.effect.implicits._

object Codegen extends IOApp {

  val maxConcurrent = 20

  def isCodgenFile(p: Path): Boolean = {
    val f = p.toFile
    if (
      f.isFile
        && f.getName.endsWith(".scala")
        && !f.getName.startsWith("Mx")
    ) {
      loadFileContents(f)
        .exists { contents =>
          contents.contains("@CompanionGen") && !contents.contains("//@NoCodegen")
        }
    } else {
      false
    }
  }

  def findProjectRoots(dir: File): fs2.Stream[IO,ProjectRoot] =
    fs2.io.file
      .Files[IO]
      .walk(dir.toPath, Seq(FileVisitOption.FOLLOW_LINKS))
//      .evalMap { f =>
//        IO.blocking {
//          println(f)
//          f
//        }
//      }
      .collect {
        case p if p.toFile.getName == "codegen.json" =>
          ProjectRoot(p.getParent.toFile)
      }

  def findCodegenScalaFiles(dir: File): fs2.Stream[IO,Path] =
    fs2.io.file
      .Files[IO]
      .walk(dir.toPath)
      .parEvalMapUnordered(maxConcurrent) { p =>
        IO.blocking {
          isCodgenFile(p) -> p
        }
      }
      .filter(_._1)
      .map(_._2)

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

  sealed trait CodeGenResult
  case class CodeGenSuccess(sourceFile: File, generatedFile: File) extends CodeGenResult
  case class CodeGenFailure(sourceFile: Option[File], failure: Throwable) extends CodeGenResult


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


  def codeGenScalaFiles(projectRoot: ProjectRoot): fs2.Stream[IO,CodeGenResult] = {
    val project = Project(projectRoot)

    val templateFactory = CodegenTemplate(project.config.template)

    println(s"finding scala files with @CompanionGen in ${projectRoot}")

    findCodegenScalaFiles(projectRoot.dir.toFile)
      .parEvalMapUnordered(maxConcurrent) { scalaFile =>
        templateFactory(scalaFile.toFile, project).run()
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

