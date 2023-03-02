package playground

import java.io.File

import a8.codegen.{CompanionGen, ScalaMetaParser}

object ScalaMetaPlay {


  def main(args: Array[String]): Unit = {

    val file: File = new File("C:\\Users\\glen\\code\\accur8\\composite\\remoteapi\\nefario\\src\\main\\scala\\a8\\nefario\\model\\Model.scala")

    val result = ScalaMetaParser.parseSourceFile(file, scala3 = false, (_,_) => CompanionGen.empty)

    result.toString

  }

}
