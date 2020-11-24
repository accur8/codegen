package a8.codegen

import java.io.File

import a8.codegen.Codegen.codeGenScalaFiles

object CodegenPlaya {

  def main(args: Array[String]): Unit = {
    codeGenScalaFiles(new File("/Users/glen/code/accur8/hermes/"))
  }


}
