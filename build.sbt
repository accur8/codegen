



scalacOptions in Global ++= Seq("-deprecation", "-unchecked", "-feature")

resolvers in Global += "a8-repo" at "https://accur8.artifactoryonline.com/accur8/all/"

publishTo in Global := Some("a8-repo-publish" at "https://accur8.artifactoryonline.com/accur8/libs-releases-local/")

credentials in Global += Credentials(Path.userHome / ".sbt" / "credentials")

scalaVersion in Global := "2.12.5"

organization in Global := "a8"

version in Global := a8.sbt_a8.versionStamp(file("."))



lazy val codegen =
  Common
    .jvmProject("a8-codegen", file("codegen"), "codegen")
    .settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "fastparse" % "0.4.4",
      )
    )

   