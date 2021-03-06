// 
// DO NOT EDIT THIS FILE IT IS MACHINE GENERATED
// 
// This file is generated from modules.conf using `a8-versions build_dot_sbt`
// 
// It was generated at 2022-03-31 17:24:04.960 -0400 by glen on stella.local
// 
// a8-versions build/versioning info follows
// 
//        build_date : Thu Sep 30 12:56:07 CDT 2021
//        build_machine : ENNS-PC
//        build_machine_ip : 127.0.1.1
//        build_java_version : 11.0.11
//        build_user : raph
//        version_number : 1.0.0-20210930_1255_master
//        project_name : a8-versions
//        build_os : Linux
// 
//      

import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.fastOptJS
import sbtcrossproject.JVMPlatform
import scalajscrossproject.JSPlatform
import sbtcrossproject.CrossType

object Common extends a8.sbt_a8.SharedSettings with a8.sbt_a8.HaxeSettings with a8.sbt_a8.SassSettings with a8.sbt_a8.dobby.DobbySettings {

  def crossProject(artifactName: String, dir: java.io.File, id: String) =
    sbtcrossproject.CrossProject(id, dir)(JSPlatform, JVMPlatform)
      .crossType(CrossType.Full)
      .settings(settings: _*)
      .settings(Keys.name := artifactName)
      .platformsSettings(JSPlatform)(jsSettings: _*)
      .platformsSettings(JVMPlatform)(jvmSettings: _*)


  def jsProject(artifactName: String, dir: java.io.File, id: String) =
    bareProject(artifactName, dir, id)
      .settings(jsSettings: _*)
      .enablePlugins(ScalaJSPlugin)

  override def jvmSettings: Seq[Def.Setting[_]] =
    super.jvmSettings ++
    Seq(
    )

  override def jsSettings: Seq[Def.Setting[_]] =
    super.jsSettings ++
    Seq(
      Compile / fastOptJS / artifactPath := crossTarget.value / "classes" / "webapp" / "scripts" / ((fastOptJS / moduleName).value + "-fastopt.js")
    )




  def readRepoUrl() = readRepoProperty("repo_url")

  lazy val repoConfigFile = new java.io.File(System.getProperty("user.home") + "/.a8/repo.properties")

  lazy val repoProperties = {
    import scala.jdk.CollectionConverters._
    val props = new java.util.Properties()
    if ( repoConfigFile.exists() ) {
      val input = new java.io.FileInputStream(repoConfigFile)
      try {
        props.load(input)
      } finally {
        input.close()
      }
      props.asScala
    } else {
      sys.error("config file " + repoConfigFile + " does not exist")
    }
  }

  def readRepoProperty(propertyName: String): String = {
    repoProperties.get(propertyName) match {
      case Some(s) =>
        s
      case None =>
        sys.error("could not find property " + propertyName + " in " + repoConfigFile)
    }
  }

  def readRepoCredentials(): Credentials = {
    val repoUrl = new java.net.URL(readRepoUrl())
    Credentials(
      readRepoProperty("repo_realm"),
      repoUrl.getHost,
      readRepoProperty("repo_user"),
      readRepoProperty("repo_password"),
    )
  }


  

}