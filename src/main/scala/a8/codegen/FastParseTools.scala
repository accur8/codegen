package a8.codegen

import fastparse.all._
import CommonOpsCopy._

object FastParseTools {

  case class SourceContext(value: String)

  trait HasSourceToken {
    var sourceToken = none[SourceToken]
  }

  case class ParserConfig(debugLogging: Boolean = false)

  case class Source(input: String, context: String)

  def parse[A](source: Source, parser: P[A]): A = {
    val parseResult = parser.parse(source.input)

    parseResult match {
      case f@ Parsed.Failure(_, _, _) =>
        throwError(
          s"""
error while parsing -- ${source.context} --
${f.msg}
${f.extra.traced.trace}
        """
        )
      case s@ Parsed.Success(v, _) =>
        v
    }

  }

  case class CaptureOriginal[A](delegate: P[A]) extends P[(A, String)] {
    override def parseRec(cfg: fastparse.core.ParseCtx[Char, String], index: Int) = {
      val parseResult = delegate.parseRec(cfg, index)
      parseResult match {
        case s: Mutable.Success[A] =>
          val original = cfg.input.slice(index, s.index)
          s.copy(value = s.value -> original)
        case f: Mutable.Failure => f
      }
    }
    override def toString = s"$delegate"
  }



  implicit class ParserOps[A](parser: P[A])(implicit config: ParserConfig) {

    def debug =
      if ( config.debugLogging )
        parser.log()
      else
        parser

    def capture: P[(A, String)] =
      CaptureOriginal(parser)
  }



  object SourceToken {

    def apply(startIndex: Int, endIndex: Int, input: fastparse.utils.ParserInput[Char,String]): SourceToken =
      SourceToken(SourceIndex(startIndex)(input), SourceIndex(endIndex)(input))

  }

  case class SourceIndex(index: Int)(val input: fastparse.utils.ParserInput[Char,String]) {
    private lazy val _rowColumn: (Int,Int) = {
      val (line,rowIndex) =
        input
          .slice(0, index)
          .lines
          .zipWithIndex
          .reduce((_,r) => r)
      (rowIndex+1) -> line.length
    }
    def row: Int = _rowColumn._1
    def column: Int = _rowColumn._2
  }


  case class SourceToken(from: SourceIndex, to: SourceIndex) {
    def token: String = from.input.slice(from.index, to.index)
    override def toString = token
  }


  def P[A](p: => Parser[A])(implicit name: sourcecode.Name): Parser[A] = {
    val delegate = fastparse.parsers.Combinators.Rule(name.value, () => p)

    new Parser[A] {
      override def parseRec(cfg: fastparse.core.ParseCtx[Char, String], index: Int) = {
        val parseResult = delegate.parseRec(cfg, index)
        parseResult match {
          case s: Mutable.Success[A] =>
            s.value match {
              case hst: HasSourceToken if hst.sourceToken.isEmpty =>
                hst.sourceToken = Some(SourceToken(SourceIndex(index)(cfg.input), SourceIndex(s.index)(cfg.input)))
              case _ => s
            }
          case _ =>
        }
        parseResult
      }
      override def toString = s"$delegate"
    }
  }

}