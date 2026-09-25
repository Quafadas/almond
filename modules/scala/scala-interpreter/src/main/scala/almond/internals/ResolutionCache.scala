package almond.internals

import java.io.File
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.util.UUID

import almond.logger.LoggerContext
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import coursierapi.{Dependency, IvyRepository, MavenRepository, Repository}

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** A persistent, on-disk cache of dependency resolution results.
  *
  * Coursier caches the artifacts it downloads, but not the outcome of resolution itself, so every
  * kernel session re-reads and re-parses the POMs of the dependencies of its `//> using dep`
  * directives (and of its `import $ivy`-s), even when all the versions are fixed and every file is
  * already in the local coursier cache.
  *
  * Ammonite has a cache for that ([[ammonite.runtime.Storage#ivyCache]]), but it refuses to write
  * to it as soon as resolution hooks are involved, as the hooks aren't part of its cache key - and
  * Almond always adds hooks. This cache lives in Almond instead, where all the inputs those hooks
  * read are known, so they can be part of the key.
  *
  * Entries are keyed by a SHA-256 of a canonical rendering of everything that can change the
  * outcome of resolution, and stored one JSON file per key. Anything that goes wrong here (an
  * unreadable or corrupted entry, a directory we can't write to, …) only costs us the cache: we
  * fall back to resolving, and never fail a cell.
  */
final class ResolutionCache(
  val dir: os.Path,
  params: ResolutionCache.Params,
  logCtx: LoggerContext
) {

  import ResolutionCache._

  private val log = logCtx(getClass)

  private val alwaysExclude: Set[(String, String)] =
    params
      .alreadyLoadedDependencies
      .map(dep => (dep.getModule.getOrganization, dep.getModule.getName))
      .toSet

  private val alwaysExcludeRendered: Vector[String] =
    alwaysExclude.toVector.map { case (org, name) => s"$org:$name" }.sorted

  log.debug(
    s"resolution-cache dir=$dir, always excluded modules (${alwaysExcludeRendered.length}): " +
      alwaysExcludeRendered.mkString(", ")
  )

  /** Resolves `dependencies` against `repositories`, reusing a previous result when we have one for
    * the same inputs.
    *
    * @param resolve
    *   how to actually resolve, called only upon a cache miss
    */
  def apply(
    dependencies: Seq[Dependency],
    repositories: Seq[Repository]
  )(resolve: () => Either[String, Seq[File]]): Either[String, Seq[File]] = {

    val effective = effectiveDependencies(dependencies)
    val inputs    = keyInputs(effective, repositories)
    val key       = sha256(inputs.mkString("\n"))
    val short     = key.take(12)

    log.debug(
      s"resolution-cache key=$key file=${entryFile(key)}, key inputs:" + System.lineSeparator() +
        inputs.mkString(System.lineSeparator())
    )

    notCacheableReason(effective, repositories) match {
      case Some(reason) =>
        log.info(s"resolution-cache not-cacheable key=$short reason=$reason")
        resolve()
      case None =>
        val start = System.nanoTime()
        read(key) match {
          case Some(files) =>
            files.find(!_.exists()) match {
              case Some(missing) =>
                log.info(s"resolution-cache stale key=$short missing=$missing")
                resolveAndStore(key, short, inputs, resolve)
              case None =>
                log.info(
                  s"resolution-cache hit key=$short files=${files.length} in ${elapsedMs(start)} ms"
                )
                Right(files)
            }
          case None =>
            resolveAndStore(key, short, inputs, resolve)
        }
    }
  }

  private def resolveAndStore(
    key: String,
    short: String,
    inputs: Seq[String],
    resolve: () => Either[String, Seq[File]]
  ): Either[String, Seq[File]] = {
    val start = System.nanoTime()
    resolve() match {
      case left @ Left(_) =>
        left
      case right @ Right(files) =>
        val ms = elapsedMs(start)
        notCacheableFilesReason(files) match {
          case Some(reason) =>
            log.info(s"resolution-cache not-cacheable key=$short reason=$reason")
          case None =>
            if (write(key, inputs, files))
              log.info(s"resolution-cache miss key=$short resolved in $ms ms, stored")
            else
              log.info(s"resolution-cache miss key=$short resolved in $ms ms, not stored")
        }
        right
    }
  }

  /** The dependencies resolution actually sees, once Almond's own resolution hooks had their say.
    *
    * Mirrors what [[ammonite.interp.DependencyLoader]] and the hooks added in
    * [[almond.amm.AmmInterpreter]] do: drop the dependencies already on the kernel class path, add
    * the automatic dependencies, then fill in the `_` versions.
    */
  private def effectiveDependencies(dependencies: Seq[Dependency]): Seq[Dependency] = {
    val kept = dependencies.filter { dep =>
      !alwaysExclude((dep.getModule.getOrganization, dep.getModule.getName))
    }
    val withAutomatic = kept.flatMap(dep => dep +: params.extraDependencies(dep))
    withAutomatic.map { dep =>
      if (dep.getVersion == "_")
        params.automaticVersions.get(dep.getModule).fold(dep)(dep.withVersion)
      else
        dep
    }
  }

  private def keyInputs(
    dependencies: Seq[Dependency],
    repositories: Seq[Repository]
  ): Seq[String] =
    Seq(
      s"formatVersion=$formatVersion",
      s"almondVersion=${almond.api.Properties.version}",
      s"almondScalaVersion=${almond.api.Properties.actualScalaVersion}",
      s"kernelScalaVersion=${params.scalaVersion}",
      s"alreadyLoadedDependencies=${alwaysExcludeRendered.length}:" +
        sha256(alwaysExcludeRendered.mkString("\n"))
    ) ++
      // sorted, so that the same dependencies listed in a different order share an entry
      dependencies.map(dep => s"dependency=${renderDependency(dep)}").sorted ++
      // not sorted: the order of the repositories decides which one an artifact comes from
      repositories.map(repo => s"repository=${renderRepository(repo)}") ++
      params.forceMavenProperties.toVector.sorted.map {
        case (k, v) => s"forceProperty=$k=$v"
      } ++
      params.mavenProfiles.toVector.sorted.map {
        case (p, enabled) => s"profile=$p=$enabled"
      }

  /** Why this resolution must not be cached, if it must not, from its inputs alone. */
  private def notCacheableReason(
    dependencies: Seq[Dependency],
    repositories: Seq[Repository]
  ): Option[String] =
    repositories
      .collectFirst {
        case repo if !isSupported(repo) =>
          s"unsupported repository type ${repo.getClass.getName}"
      }
      .orElse {
        dependencies.collectFirst {
          case dep if dep.getVersion == "_" =>
            s"no automatic version for ${renderModule(dep.getModule)}"
          case dep if !isConcreteVersion(dep.getVersion) =>
            s"non-concrete version '${dep.getVersion}' for ${renderModule(dep.getModule)}"
          case dep if isChangingVersion(dep.getVersion) =>
            s"changing version '${dep.getVersion}' for ${renderModule(dep.getModule)}"
        }
      }

  /** Why this resolution must not be cached, if it must not, from what it resolved to.
    *
    * `loadIvy` only hands us files back, not the coursier resolution, so we can't ask coursier
    * whether an artifact is changing - we go by the snapshot marker in the path instead.
    */
  private def notCacheableFilesReason(files: Seq[File]): Option[String] =
    files.collectFirst {
      case f if f.getAbsolutePath.contains(snapshotMarker) =>
        s"changing artifact ${f.getAbsolutePath}"
      case f if !f.exists() =>
        s"resolved file does not exist: ${f.getAbsolutePath}"
    }

  private def entryFile(key: String): os.Path =
    dir / s"$key.json"

  private def read(key: String): Option[Seq[File]] = {
    val file = entryFile(key)
    Try {
      if (os.isFile(file)) {
        val entry = readFromArray(os.read.bytes(file))(Entry.codec)
        if (entry.formatVersion == formatVersion)
          Some(entry.files.map(new File(_)))
        else
          None
      }
      else
        None
    } match {
      case Success(res) => res
      case Failure(ex) =>
        log.debug(s"Ignoring unusable resolution cache entry $file", ex)
        None
    }
  }

  private def write(key: String, inputs: Seq[String], files: Seq[File]): Boolean = {
    val file = entryFile(key)
    Try {
      val entry = Entry(
        formatVersion = formatVersion,
        createdAt = Instant.now().toString,
        keyInputs = inputs.toList,
        files = files.map(_.getAbsolutePath).toList
      )
      val content = writeToArray(entry, WriterConfig.withIndentionStep(2))(Entry.codec)
      // written aside then moved in place, so that kernels running concurrently
      // never read a half-written entry
      val tmp = dir / s".$key-${UUID.randomUUID()}.tmp"
      os.write.over(tmp, content, createFolders = true)
      try os.move(tmp, file, replaceExisting = true, atomicMove = true)
      catch {
        case _: Throwable =>
          os.move(tmp, file, replaceExisting = true)
      }
    } match {
      case Success(_) => true
      case Failure(ex) =>
        log.debug(s"Could not write resolution cache entry $file", ex)
        false
    }
  }
}

object ResolutionCache {

  /** Bump when the meaning of a key, or the layout of an entry, changes. */
  def formatVersion: Int = 1

  /** Everything Almond's resolution hooks read, so that the cache key can account for them.
    *
    * @param extraDependencies
    *   the automatic dependencies a dependency pulls in (`--auto-dependency`)
    * @param automaticVersions
    *   the versions `_` stands for (`--auto-version`)
    * @param forceMavenProperties
    *   `--force-property`
    * @param mavenProfiles
    *   `--profile`
    * @param alreadyLoadedDependencies
    *   the dependencies already on the kernel class path, excluded from every resolution
    * @param scalaVersion
    *   the Scala version the kernel runs, which the dependencies of a session were resolved against
    */
  final case class Params(
    extraDependencies: Dependency => Seq[Dependency] = _ => Nil,
    automaticVersions: Map[coursierapi.Module, String] = Map.empty,
    forceMavenProperties: Map[String, String] = Map.empty,
    mavenProfiles: Map[String, Boolean] = Map.empty,
    alreadyLoadedDependencies: Seq[Dependency] = Nil,
    scalaVersion: String = ""
  )

  private[almond] final case class Entry(
    formatVersion: Int,
    createdAt: String,
    keyInputs: List[String],
    files: List[String]
  )

  private[almond] object Entry {
    implicit val codec: JsonValueCodec[Entry] =
      JsonCodecMaker.make
  }

  private def snapshotMarker = "SNAPSHOT"

  private def elapsedMs(startNanos: Long): Long =
    (System.nanoTime() - startNanos) / 1000000L

  private def sha256(input: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
      .digest(input.getBytes(StandardCharsets.UTF_8))
    val b = new StringBuilder(digest.length * 2)
    for (byte <- digest) {
      val v = byte & 0xff
      if (v < 16) b.append('0')
      b.append(Integer.toHexString(v))
    }
    b.result()
  }

  /** Whether a version pins one specific release, rather than letting resolution pick one. */
  private[almond] def isConcreteVersion(version: String): Boolean =
    version.nonEmpty &&
    !version.startsWith("latest.") &&
    !version.exists(c => c == '[' || c == ']' || c == '(' || c == ')') &&
    !version.endsWith("+") &&
    version != "_"

  private[almond] def isChangingVersion(version: String): Boolean =
    version.contains(snapshotMarker)

  private def isSupported(repository: Repository): Boolean =
    repository match {
      case _: MavenRepository => true
      case _: IvyRepository   => true
      case _                  => false
    }

  /** Strips the `user:password@` part of a URL, if any.
    *
    * Credentials never change what gets resolved, and have no business being in a key or in a file
    * on disk.
    */
  private[almond] def withoutUserInfo(url: String): String = {
    val schemeEnd = url.indexOf("://")
    if (schemeEnd < 0) url
    else {
      val authorityStart = schemeEnd + "://".length
      val authorityEnd = {
        val idx = url.indexOf('/', authorityStart)
        if (idx < 0) url.length else idx
      }
      val at = url.lastIndexOf('@', authorityEnd - 1)
      if (at < authorityStart) url
      else url.substring(0, authorityStart) + url.substring(at + 1)
    }
  }

  private[almond] def renderModule(module: coursierapi.Module): String = {
    val attributes = module
      .getAttributes
      .asScala
      .toVector
      .sortBy(_._1)
      .map { case (k, v) => s";$k=$v" }
      .mkString
    s"${module.getOrganization}:${module.getName}$attributes"
  }

  private[almond] def renderDependency(dependency: Dependency): String = {
    val exclusions = dependency
      .getExclusions
      .asScala
      .toVector
      .map(e => s"${e.getKey}:${e.getValue}")
      .sorted
      .mkString(",")
    val publication = Option(dependency.getPublication)
      .filter(!_.isEmpty)
      .fold("") { pub =>
        s" publication=${pub.getName}|${pub.getType}|${pub.getExtension}|${pub.getClassifier}"
      }
    val overrides = dependency
      .getOverrides
      .asScala
      .toVector
      .map { case (k, v) => s"$k=>$v" }
      .sorted
      .mkString(",")
    s"${renderModule(dependency.getModule)}:${dependency.getVersion}" +
      s" type=${dependency.getType}" +
      s" classifier=${dependency.getClassifier}" +
      s" configuration=${dependency.getConfiguration}" +
      s" transitive=${dependency.isTransitive}" +
      s" exclusions=[$exclusions]" +
      publication +
      s" overrides=[$overrides]"
  }

  /** Renders a repository, credentials left out - both the [[coursierapi.Credentials]] attached to
    * it, and any `user:password@` in its URL.
    */
  private[almond] def renderRepository(repository: Repository): String =
    repository match {
      case m: MavenRepository =>
        s"maven:${withoutUserInfo(m.getBase)}"
      case i: IvyRepository =>
        val metadataPattern = Option(i.getMetadataPattern).getOrElse("")
        s"ivy:${withoutUserInfo(i.getPattern)}|${withoutUserInfo(metadataPattern)}" +
          s"|dropInfoAttributes=${i.getDropInfoAttributes}"
      case other =>
        // not cached - see notCacheableReason - so this only needs to be stable, not exact
        s"unsupported:${other.getClass.getName}"
    }

}
