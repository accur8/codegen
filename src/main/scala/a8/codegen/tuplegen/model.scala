package a8.codegen.tuplegen

object model {

  case class Generator(typeClassName: String) {

    val tupleDefs =
      (1 to 22) map {
        TupleDef(_, this)
      }

  }

  case class TupleDef(arity: Int, generator: Generator) {

    lazy val args = (1 to arity).map(a => Arg(a, generator))

    val typeName = s"(${args.map(_.typeName).mkString(",")})"
    val typeClassedTypeName = s"(${args.map(_.typeClassedTypeName).mkString(",")})"
  }

  case class Arg(index: Int, generator: Generator) {
    val typeName = "A" + index
    val typeClassedTypeName = typeName + ":" + generator.typeClassName
    val valueVal = typeName.toLowerCase
    val offsetVal = typeName.toLowerCase + "o"
    val tupleValName = s"_${index}"
  }

}
