package a8.codegen.tuplegen


import a8.codegen.tuplegen.model.{Generator, TupleDef}
import a8.codegen.CommonOpsCopy._

object MessagePackCodecTupleCodeGen {

  def main(args: Array[String]) = {
    println(
      Generator("Codec")
        .tupleDefs
        .map(generateForTuple)
        .mkString("\n\n")
        .indent("  ")
    )
  }

  def generateForTuple(tupleDef: TupleDef) = {
    import tupleDef._
s"""
implicit def tuple${arity}[${typeClassedTypeName}]: Codec[${typeName}] =
  new Codec[${typeName}] {
    def pack(t: ${typeName}, packer: Packer): Unit = {
${
      args
        .map(a => s"packer.pack(${a.tupleValName})")
        .mkString("\n")
        .indent("      ")
}
    }
    def unpack(unpacker: Unpacker): A =
      (
${
      args
        .map(a => s"unpacker.unpacker[${a.typeName}]")
        .mkString(",\n")
        .indent("        ")
}
      )
    def toMachineString(a: A): Chord = (
      Chord.leftParen
${
      args
        .map(a => s"~ Codec[${a.typeName}].toMachineString(${a.tupleValName}) ~ Chord.comma")
        .mkString("\n")
        .indent("      ")
}
        ~ Chord.rightParen
    )
  }
""".trim
  }

}
