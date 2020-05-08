package playground

import java.io.File

import a8.codegen.Codegen

object Run extends App {

  val dir = new File("/Users/flow/code/odin/mugatu/server")

  Codegen.codeGenScalaFiles(dir)


}
