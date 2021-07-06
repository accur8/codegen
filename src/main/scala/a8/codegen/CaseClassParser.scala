package a8.codegen


import a8.codegen.FastParseTools.ParserConfig
import a8.codegen.CaseClassAst.Import
import a8.codegen.CompanionGen.CompanionGenResolver
import a8.codegen.ProjectConfig.Anno
import a8.codegen.{CaseClassAst => ast, FastParseTools => fpt}
import fastparse.all._

class CaseClassParser(file: java.io.File, companionGenResolverFn: (String,Anno) => CompanionGen)(implicit config: ParserConfig) {

  val comment: P[Unit] =
    P( "//" ~ CharsWhile(_ != '\n') )

  val ws = P( ws1.? )
  val ws1 = P( (CharsWhile(_.isWhitespace, min=1) | comment).rep(1) )
  val NL = P("\n")

  val Package: P[String] =
    P(k0("package") ~ Line.! ~ NL)

  val Line =
    P(CharsWhile(_ != '\n').! ~ NL)

  val SourceFile =
    P(Package ~ Import.rep ~ CaseClass.rep ~ Token.rep ~ End)
      .map { case (pakkage, imports, caseClasses) =>
        ast.SourceFile(pakkage, imports, caseClasses)
      }
//      .log()

  val Import: P[Import] =
    P(k0("import") ~ Line)
      .map(ast.Import)
//      .log()

  val Token: P0 =
    P(ws ~ !CompanionGen ~ CharsWhile(!_.isWhitespace) ~ ws)
//      .log()

  val CompanionGen: P[Anno] =
    P(k0("@CompanionGen") ~ ws ~ ("(" ~ ws ~ CompanionGenParms ~ ws ~ ")").?)
      .map { parms: Option[Seq[(String, Boolean)]] =>
        Anno(
          parms
            .toSeq
            .flatten
            .map(t => t.copy(_2 = t._2))
            .toMap
        )
      }

  val CompanionGenParms: P[Seq[(String,Boolean)]] =
    P(CompanionGenParm.rep(sep = Comma))

  val CompanionGenParm: P[(String,Boolean)] =
    P(Name ~ ws ~ "=" ~ ws ~ TrueFalse)

  val TrueFalse: P[Boolean] = P(k0("true").map(_ => true) | k0("false").map(_ => false))

  val Comma =
    P(ws ~ "," ~ ws)

  val Dot =
    P(ws ~ "." ~ ws)


  val CaseClass: P[ast.CaseClass] =
    P(Token.rep ~ ws ~ CompanionGen ~/ ws ~ Annotation.rep ~ ws ~ k0("case") ~ ws ~ k0("class") ~ ws ~ Name ~ ws ~ "(" ~ ws ~/ Property.rep(sep=Comma) ~ Comma.? ~ ws ~ ")" ~ ws )
      .map { case (companionGen, annotations, name, props) =>
        ast.CaseClass(file, name, props, companionGenResolverFn(name, companionGen), annotations)

      }
//      .log()


  val Annotation: P[ast.Annotation] =
    P(ws ~ "@" ~ Name ~ ws ~ ("(" ~ AnnotationParm.rep(sep = Comma) ~ ")").?)
      .map { case (name, parms) =>
        ast.Annotation(name, parms.getOrElse(Nil))
      }
//      .log()

  val AnnotationParm: P[ast.AnnotationParm] =
    P(ws ~ Name ~ ws ~ "=" ~ ws ~ Atom.!)
      .map { case (name, value) =>
        ast.AnnotationParm(name, value)
      }
//      .log()

  val Property =
    P(ws ~ Annotation.rep ~ ws ~ (Name|QuotedName) ~ ws ~ ":" ~ ws ~ TypeName ~ ws ~ ("=" ~ ws ~ Expr.!).?)
      .map { case (annotations, name, typeName, defaultValue) =>
        ast.Property(name, typeName, defaultValue, annotations)
      }
//      .log()


  val Expr: P0 = P( Atom )

  val Atom: P0 =
    P(
      Number
      | Bool
      | TripleQuoted.String
      | StringLit
      | (k0("new") ~ Name.rep(sep=Dot))
      | Parens
      | FunctionCall
    )
      .map(_ => ())
//      .log()

  val Bool: P0 =
    P(k0("true") | k0("false"))

  val Parens: P0 =
    P("(" ~ ws ~ Expr ~ ws ~ ")").map( _ => () )

  val FunctionCall: P0 =
    P(Name.rep(sep=Dot) ~ FunctionSuffix.?).map( _ => () )

  val FunctionSuffix: P0 =
    P( "(" ~ ws ~ Expr.rep(sep=Comma) ~ ws ~ ")" )
      .map( _ => () )

  val TypeName: P[ast.TypeName] =
    P(
      (Name ~ ws ~ ("[" ~ ws ~ TypeName.rep(min=1, sep=Comma) ~ ws ~ "]" ~ ws).?)
        .map { case (n, args) => ast.TypeName(n, args.getOrElse(Nil)) }
      | ("(" ~ ws ~ TypeName.rep(min=1, sep=Comma) ~ ws ~ ")").!.map(n => ast.TypeName(n))
    )

  val Name: P[String] =
    P( (!Keyword) ~ (IdentFirstChar ~ IdentSecondChar.rep).! ~ ws )
//      .log()

  val QuotedName: P[String] =
    P( ("`" ~ (IdentFirstChar ~ IdentSecondChar.rep) ~ "`").! ~ ws )

  val IdentFirstChar =
    P( CharsWhile(ch => ch.isLetter || ch == '_' ) )

  val IdentSecondChar =
    P( CharsWhile(ch => ch.isLetter || ch == '.' || ch == '$' || ch == '#' || ch == '_' || ch.isDigit) )




  object TripleQuoted {
    val TQ =
      P( "\"\"\"" )
//        .log()

    /**
     * Helper to quickly gobble up large chunks of un-interesting
     * characters. We break out conservatively, even if we don't know
     * it's a "real" escape sequence: worst come to worst it turns out
     * to be a dud and we go back into a CharsChunk next rep
     */
    val StringChars = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
    val NonTripleQuoteChar = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\\\$\n") )

    val TripleChars =
      P( (StringChars | NonTripleQuoteChar).rep )
//        .log()

    val TripleTail =
      P( TQ ~ "\"".rep )
//        .log()

    val String =
      P(
        TQ ~/ TripleChars ~ TripleTail
      )
//        .log()
  }

  val Keyword =
    P(
      k0("case")
      | k0("class")
      | k0("false")
      | k0("new")
      | k0("true")
      | k0("type")
      | k0("import")
    )

  val Path: P[String] =
    Name.rep(min=1, sep=".").!

  def k0(s: String): P[Unit] =
    kword(s).map(_ => ())

  def kword(s: String): P[String] =
    P( ws ~ IgnoreCase(s).! ~ !LetterDigitUnderscore ~ ws )(sourcecode.Name(s"`$s`"))

  val LetterDigitUnderscore = P( CharPred(c => c.isLetter || c.isDigit || c == '_' ) )


  val StringLit: P0 =
    QuotedString('"').map(_ => ())

  def QuotedString(quoteChar: Char) = {
    val quoteCharStr = quoteChar.toString
    val Body = P(CharsWhile(ch => ch != quoteChar && ch != '\\') | ("\\" ~ EscapedChar)).rep
    P(ws ~ quoteCharStr ~ Body.! ~ quoteCharStr ~ ws)
  }

  val escapedCharsMap =
    Map(
      "\'" -> "'",
      "\"" -> "\"",
      "b" -> "\b",
      "f" -> "\f",
      "n" -> "\n",
      "r" -> "\r",
      "t" -> "\t"
    )

  val escapedCharsSet = escapedCharsMap.keys.map(_.charAt(0)).toSet

  val EscapedChar: P[String] =
    P( CharPred(escapedCharsSet.contains) ).!
      .map(escapedCharsMap.apply)

  val Number: P0 =
    P( ("-"|"+").? ~ Digits ~ ("." ~ Digits.?).? )
      .map( _ => () )

  val Digits: P0 = P( CharsWhile(_.isDigit) )

}
