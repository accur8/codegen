package a8.codegen


import a8.codegen.CaseClassAst.TypeName
import a8.codegen.CaseClassAst.TypeName.{RegularTypeName, TupleTypeName}
import cats.parse.{Numbers, Parser0, Parser => P}

object TypeNameParser extends App {

  lazy val ws: P[Unit] = P.charIn(" \t\r\n").void
  lazy val ws0: Parser0[Unit] = ws.rep0.void

  def sep(ch: Char): P[Unit] =
    (ws0.with1.soft ~ P.char(ch) ~ ws0).void

  lazy val parser: P[TypeName] =
    P.recursive[TypeName] { recurse =>

      lazy val commaSeparatedTypes = recurse.repSep(sep(','))

      lazy val regularTypeName: P[TypeName] =
        (name ~ (sep('[') *> commaSeparatedTypes <* sep(']')).? <* ws0)
          .map { case (n, args) =>
              RegularTypeName(n, args.map(_.toList).getOrElse(Nil))
          }

      lazy val tupleTypeName: P[TypeName] =
        (sep('(') *> commaSeparatedTypes <* sep(')') <* ws0)
          .map { l =>
            TupleTypeName(l.toList)
          }

      regularTypeName.orElse(tupleTypeName)

    }

  lazy val name: P[String] = P.charsWhile(ch => ch.isLetterOrDigit || ch == '_' || ch == '.') <* ws0



  println(parser.parseAll("Hello[World]"))
  println(parser.parseAll("(Hello[World],Fred)"))

}
