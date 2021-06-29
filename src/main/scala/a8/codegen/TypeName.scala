package a8.codegen

case class TypeName(fullName: String) {

  lazy val shortName = fullName.split("\\.").last

  override def toString: String =
    fullName

}
