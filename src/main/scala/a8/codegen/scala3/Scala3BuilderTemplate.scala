package a8.codegen.scala3


import a8.codegen.CaseClassAst.{CaseClass, TypeName}
import a8.codegen.BuilderTemplate

object Scala3BuilderTemplate {

  lazy val messagePackTemplate =
    new BuilderTemplate(
      "messagePackCodec",
      TypeName("a8.wsjdbc.codec.Codec"),
      TypeName("a8.wsjdbc.codec.CodecBuilder"),
      generateFor = _.messagePack,
    )

  lazy val circeTemplate =
    new BuilderTemplate(
      "circeCodec",
      TypeName("io.circe.Codec"),
      TypeName("a8.shared.CirceCodecBuilder"),
      generateFor = _.circeCodec,
    )

  lazy val jsonObjectTemplate =
    new BuilderTemplate(
      "jsonCodec",
      TypeName("a8.shared.json.JsonTypedCodec"),
      TypeName("a8.shared.json.JsonObjectCodecBuilder"),
      generateFor = _.jsonCodec,
    ) {

      override def resolveTypeClassName(caseClass: CaseClass): String =
        s"${typeClassName.fullName}[${caseClass.name},a8.shared.json.ast.JsObj]"

    }

  lazy val templates = List(messagePackTemplate, Scala3JdbcMapperTemplate, Scala3QubesMapperTemplate, circeTemplate, jsonObjectTemplate)

}
