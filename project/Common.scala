import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}

object Common extends a8.sbt_a8.SharedSettings with a8.sbt_a8.HaxeSettings {

  def crossProject(artifactName: String, dir: java.io.File, id: String) =
    CrossProject(id, dir, CrossType.Full)
      .settings(settings: _*)
      .settings(Keys.name := artifactName)
      .jsSettings(jsSettings: _*)
      .jvmSettings(jvmSettings: _*)


  def jsProject(artifactName: String, dir: java.io.File, id: String) =
    bareProject(artifactName, dir, id)
      .settings(jsSettings: _*)
      .enablePlugins(ScalaJSPlugin)

  override def jvmSettings: Seq[Def.Setting[_]] =
    super.jvmSettings ++
    Seq(
    )

}