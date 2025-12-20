import sbt._
import Keys._
import sbt.librarymanagement.{Artifact, Caller, ModuleID, ModuleReport, UpdateReport}

object DependenciesJson {

  val dependenciesJsonTask = taskKey[File]("Generate dependencies.json file")

  def dependencyResourceSettings(
    generateFullDependenciesSetting: SettingKey[Boolean],
    repoConfigFile: => java.io.File,
    readRepoUrl: => String
  ): Seq[Def.Setting[_]] = Seq(
    dependenciesJsonTask := {
      val modules = runtimeModules((Runtime / update).value)
      val targetDir = target.value
      val depsFile = targetDir / "dependencies.json"
      sbt.IO.write(depsFile, fullDependenciesJson(modules, repoConfigFile, readRepoUrl))
      depsFile
    },
    Compile / resourceGenerators += Def.task {
      if (generateFullDependenciesSetting.value) {
        val modules = runtimeModules((Runtime / update).value)
        val managedDir = (Compile / resourceManaged).value
        val depsFile = managedDir / "dependencies.json"
        sbt.IO.write(depsFile, fullDependenciesJson(modules, repoConfigFile, readRepoUrl))
        Seq(depsFile)
      } else {
        Seq.empty
      }
    },
    artifacts := {
      val orig = artifacts.value
      if (generateFullDependenciesSetting.value) {
        orig :+ Artifact(moduleName.value, "json", "json", "dependencies")
      } else {
        orig
      }
    },
    packagedArtifacts := {
      val orig = packagedArtifacts.value
      if (generateFullDependenciesSetting.value) {
        val depsArtifact = Artifact(moduleName.value, "json", "json", "dependencies")
        orig.updated(depsArtifact, dependenciesJsonTask.value)
      } else {
        orig
      }
    }
  )

  private def runtimeModules(report: UpdateReport): Seq[ModuleReport] =
    report.configurations
      .find(_.configuration.name == "runtime")
      .toSeq
      .flatMap(_.modules)
      .groupBy(_.module)  // Group by ModuleID (organization + name + version)
      .values
      .map(_.head)  // Take first from each group - within a single configuration,
                    // modules with the same ModuleID have identical artifacts and metadata.
                    // Note: callers may differ but we only report one dependency per module.
      .toSeq
      .sortBy(m => (m.module.organization, m.module.name, m.module.revision))

  private def fullDependenciesJson(modules: Seq[ModuleReport], repoConfigFile: java.io.File, readRepoUrl: => String): String = {
    val items = modules.map(m => fullModuleJson(m, repoConfigFile, readRepoUrl))
    renderJson(JObject(Seq("dependencies" -> JArray(items)))) + "\n"
  }

  private def fullModuleJson(moduleReport: ModuleReport, repoConfigFile: java.io.File, readRepoUrl: => String): JObject =
    JObject(filterEmptyFields(Seq(
      "moduleId" -> moduleIdJson(moduleReport.module),
      "resolver" -> optionalString(moduleReport.resolver),
      "artifactResolver" -> optionalString(moduleReport.artifactResolver),
      "evicted" -> JBoolean(moduleReport.evicted),
      "evictedData" -> optionalString(moduleReport.evictedData),
      "evictedReason" -> optionalString(moduleReport.evictedReason),
      "branch" -> optionalString(moduleReport.branch),
      "artifacts" -> JArray(moduleReport.artifacts.map { case (artifact, file) =>
        fullArtifactJson(moduleReport.module, artifact, file, repoConfigFile, readRepoUrl)
      }),
      "missingArtifacts" -> JArray(moduleReport.missingArtifacts.map(artifact => artifactJson(artifact))),
      "callers" -> JArray(moduleReport.callers.map(caller => callerJson(caller)))
    )))

  private def moduleIdJson(moduleId: ModuleID): JObject =
    JObject(filterEmptyFields(Seq(
      "organization" -> JString(moduleId.organization),
      "artifact" -> JString(moduleId.name),
      "version" -> JString(moduleId.revision),
      "isChanging" -> JBoolean(moduleId.isChanging),
      "isTransitive" -> JBoolean(moduleId.isTransitive),
      "isForce" -> JBoolean(moduleId.isForce)
    )))

  private def artifactJson(artifact: Artifact): JObject =
    JObject(artifactFields(artifact))

  private def artifactFields(artifact: Artifact): Seq[(String, JsonValue)] =
    Seq(
      "name" -> JString(artifact.name),
      "type" -> JString(artifact.`type`),
      "extension" -> JString(artifact.extension),
      "classifier" -> optionalString(artifact.classifier)
    )

  private def fullArtifactJson(module: ModuleID, artifact: Artifact, file: java.io.File, repoConfigFile: java.io.File, readRepoUrl: => String): JObject = {
    val sha256 = calculateSha256(file)
    val artifactUrl = artifact.url.orElse(constructRepoUrl(module, artifact, repoConfigFile, readRepoUrl))
    JObject(filterEmptyFields(Seq(
      "name" -> JString(artifact.name),
      "type" -> JString(artifact.`type`),
      "extension" -> JString(artifact.extension),
      "classifier" -> optionalString(artifact.classifier),
      "url" -> optionalUrl(artifactUrl),
      "checksum" -> JNull,  // Checksums not directly available on Artifact
      "sha256" -> JString(sha256)
    )))
  }

  private def constructRepoUrl(module: ModuleID, artifact: Artifact, repoConfigFile: java.io.File, readRepoUrl: => String): Option[java.net.URL] = {
    try {
      // For artifacts from the a8 organization or when repo is configured, construct the Maven repository URL
      if (repoConfigFile.exists()) {
        val baseUrl = readRepoUrl.stripSuffix("/")
        val orgPath = module.organization.replace('.', '/')
        val classifierSuffix = artifact.classifier.map(c => s"-$c").getOrElse("")
        val urlString = s"$baseUrl/$orgPath/${module.name}/${module.revision}/${artifact.name}-${module.revision}$classifierSuffix.${artifact.extension}"
        Some(new java.net.URL(urlString))
      } else {
        None
      }
    } catch {
      case _: Exception => None
    }
  }

  private def calculateSha256(file: java.io.File): String = {
    import java.security.MessageDigest
    import java.io.{FileInputStream, BufferedInputStream}

    val buffer = new Array[Byte](8192)
    val sha256 = MessageDigest.getInstance("SHA-256")
    val fis = new FileInputStream(file)
    val bis = new BufferedInputStream(fis)

    try {
      var read = bis.read(buffer)
      while (read > -1) {
        sha256.update(buffer, 0, read)
        read = bis.read(buffer)
      }
      sha256.digest().map("%02x".format(_)).mkString
    } finally {
      bis.close()
      fis.close()
    }
  }

  private def optionalUrl(url: Option[java.net.URL]): JsonValue =
    url.map(u => JString(u.toString)).getOrElse(JNull)

  private def callerJson(caller: Caller): JObject =
    JObject(filterEmptyFields(Seq(
      "module" -> moduleIdJson(caller.caller),
      "isTransitiveDependency" -> JBoolean(caller.isTransitiveDependency)
    )))

  private def filterEmptyFields(fields: Seq[(String, JsonValue)]): Seq[(String, JsonValue)] =
    fields.filter {
      case (_, JNull) => false
      case (_, JBoolean(false)) => false
      case (_, JArray(items)) if items.isEmpty => false
      case _ => true
    }

  private def optionalString(value: Option[String]): JsonValue =
    value.map(JString).getOrElse(JNull)

  private sealed trait JsonValue
  private case class JObject(fields: Seq[(String, JsonValue)]) extends JsonValue
  private case class JArray(items: Seq[JsonValue]) extends JsonValue
  private case class JString(value: String) extends JsonValue
  private case class JBoolean(value: Boolean) extends JsonValue
  private case object JNull extends JsonValue

  private def renderJson(value: JsonValue, indent: Int = 0): String = {
    val pad = "  " * indent
    value match {
      case JNull => "null"
      case JBoolean(bool) => if (bool) "true" else "false"
      case JString(str) => jsonString(str)
      case JArray(items) if items.isEmpty => "[]"
      case JArray(items) =>
        val rendered = items.map(v => "\n" + "  " * (indent + 1) + renderJson(v, indent + 1)).mkString(",")
        "[" + rendered + "\n" + pad + "]"
      case JObject(fields) if fields.isEmpty => "{}"
      case JObject(fields) =>
        val rendered = fields.map { case (key, jsonValue) =>
          "\n" + "  " * (indent + 1) + jsonString(key) + ": " + renderJson(jsonValue, indent + 1)
        }.mkString(",")
        "{" + rendered + "\n" + pad + "}"
    }
  }

  private def jsonString(value: String): String = {
    val escaped = value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c.isControl => "\\u%04x".format(Int.box(c.toInt))
      case c => c.toString
    }
    "\"" + escaped + "\""
  }

}
