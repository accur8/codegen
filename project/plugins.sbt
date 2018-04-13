

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.22")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.1")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")

resolvers += "a8-sbt-plugins" at "https://accur8.artifactoryonline.com/accur8/sbt-plugins/"
credentials += Credentials(Path.userHome / ".sbt" / "credentials")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.4.0")

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.21"
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")


addSbtPlugin("a8" % "sbt-a8" % "1.1.0-20180412_1831")

      