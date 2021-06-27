package a8.codegen.tuplegen


import a8.codegen.tuplegen.model.{Generator, TupleDef}
import a8.codegen.CommonOpsCopy._

object RowReaderTupleCodeGen {

  def main(args: Array[String]) = {
    println(
      Generator("RowReader")
        .tupleDefs
        .map(generateForTuple)
        .mkString("\n\n")
        .indent("  ")
    )
  }

  case class Arg(index: Int) {
    val typeName = "A" + index
    val readerTypeName = typeName + ":RowReader"
    val valueVal = typeName.toLowerCase
    val offsetVal = typeName.toLowerCase + "o"
  }

  def generateForTuple(tupleDef: TupleDef) = {
    import tupleDef._
s"""
implicit def tuple${arity}[${tupleDef.typeClassedTypeName}]: RowReader[${tupleDef.typeName}] =
  new RowReader[${tupleDef.typeName}] {
    override def rawRead(row: Row, index: Int): (${tupleDef.typeName}, Int) = {
      var offset = 0
${
      args
        .map { a =>
s"""
val (${a.valueVal}, ${a.offsetVal}) = RowReader[${a.typeName}].rawRead(row, offset+index)
offset += ${a.offsetVal}
""".trim
        }
        .mkString("\n")
        .indent("      ")
}
      (${args.map(_.valueVal).mkString(",")}) -> offset
    }
  }
""".trim
  }

}
