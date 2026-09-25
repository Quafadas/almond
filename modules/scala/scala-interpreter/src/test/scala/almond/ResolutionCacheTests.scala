package almond

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.charset.StandardCharsets

import almond.internals.ResolutionCache
import almond.logger.{Level, LoggerContext}
import coursierapi.{Credentials, Dependency, MavenRepository, Module, Repository}
import utest._

object ResolutionCacheTests extends TestSuite {

  private def logCtxTo(out: ByteArrayOutputStream): LoggerContext =
    LoggerContext.printStream(
      Level.Info,
      new PrintStream(out, true, "UTF-8"),
      colored = false
    )

  private def captured(out: ByteArrayOutputStream): String =
    new String(out.toByteArray, StandardCharsets.UTF_8)

  private def entries(dir: os.Path): Seq[String] =
    if (os.exists(dir))
      os.list(dir).map(_.last).filter(_.endsWith(".json")).sorted
    else
      Nil

  private def touch(path: os.Path): File = {
    os.write.over(path, "", createFolders = true)
    path.toIO
  }

  private val central = Repository.central()

  private def dep(org: String, name: String, version: String): Dependency =
    Dependency.of(org, name, version)

  /** A cache that resolves to `files`, counting how many times it actually had to. */
  private final class Fixture(
    dir: os.Path,
    params: ResolutionCache.Params = ResolutionCache.Params()
  ) {
    val out                      = new ByteArrayOutputStream
    val cache                    = new ResolutionCache(dir, params, logCtxTo(out))
    private var resolutionCount0 = 0
    def resolutionCount: Int     = resolutionCount0
    def log: String              = captured(out)
    def run(
      dependencies: Seq[Dependency],
      repositories: Seq[Repository] = Seq(central),
      files: => Seq[File] = Nil
    ): Either[String, Seq[File]] =
      cache(dependencies, repositories) { () =>
        resolutionCount0 += 1
        Right(files)
      }
  }

  private def interpreter(
    cacheDir: Option[os.Path],
    logCtx: LoggerContext
  ): ScalaInterpreter =
    new ScalaInterpreter(
      params = TestUtil.interpreterParams.copy(
        resolutionCache = cacheDir.nonEmpty,
        resolutionCacheDir = cacheDir
      ),
      logCtx = logCtx
    )

  // pure Java, no transitive dependencies, and not on the kernel class path
  private val testDependencyCell =
    """//> using dep commons-io:commons-io:2.16.1
      |val n = classOf[org.apache.commons.io.IOUtils].getName
      |""".stripMargin

  val tests = Tests {

    test("miss then hit across sessions") {
      val dir = os.temp.dir(prefix = "almond-resolution-cache-test")

      val firstOut = new ByteArrayOutputStream
      val first    = interpreter(Some(dir), logCtxTo(firstOut))
      val firstRes = first.execute(testDependencyCell)
      assert(firstRes.success)
      assert(captured(firstOut).contains("resolution-cache miss"))
      assert(captured(firstOut).contains("stored"))
      assert(entries(dir).length == 1)

      val secondOut = new ByteArrayOutputStream
      val second    = interpreter(Some(dir), logCtxTo(secondOut))
      val secondRes = second.execute(testDependencyCell)
      assert(secondRes.success)
      assert(captured(secondOut).contains("resolution-cache hit"))
      assert(!captured(secondOut).contains("resolution-cache miss"))
      assert(entries(dir).length == 1)

      // the class of the dependency is usable in both sessions
      assert(firstRes == secondRes)
    }

    test("import $ivy too") {
      val dir  = os.temp.dir(prefix = "almond-resolution-cache-test")
      val cell = "import $ivy.`commons-io:commons-io:2.16.1`"

      val firstOut = new ByteArrayOutputStream
      assert(interpreter(Some(dir), logCtxTo(firstOut)).execute(cell).success)
      assert(captured(firstOut).contains("resolution-cache miss"))
      assert(entries(dir).length == 1)

      val secondOut = new ByteArrayOutputStream
      assert(interpreter(Some(dir), logCtxTo(secondOut)).execute(cell).success)
      assert(captured(secondOut).contains("resolution-cache hit"))
    }

    test("corrupt entry is a miss") {
      val dir = os.temp.dir(prefix = "almond-resolution-cache-test")

      val firstOut = new ByteArrayOutputStream
      assert(interpreter(Some(dir), logCtxTo(firstOut)).execute(testDependencyCell).success)
      val entry = entries(dir) match {
        case Seq(single) => dir / single
        case other       => sys.error(s"Expected one cache entry, got $other")
      }

      os.write.over(entry, "{ this is not JSON")

      val secondOut = new ByteArrayOutputStream
      val res       = interpreter(Some(dir), logCtxTo(secondOut)).execute(testDependencyCell)
      assert(res.success)
      assert(captured(secondOut).contains("resolution-cache miss"))
      assert(!captured(secondOut).contains("resolution-cache hit"))
    }

    test("flag off") {
      val dir = os.temp.dir(prefix = "almond-resolution-cache-test")
      val out = new ByteArrayOutputStream
      val res = interpreter(None, logCtxTo(out)).execute(testDependencyCell)
      assert(res.success)
      assert(entries(dir).isEmpty)
      assert(!captured(out).contains("resolution-cache"))
    }

    test("not cacheable") {

      def check(
        dependencies: Seq[Dependency],
        params: ResolutionCache.Params,
        expectedReason: String
      ): Unit = {
        val dir     = os.temp.dir(prefix = "almond-resolution-cache-test")
        val fixture = new Fixture(dir, params)
        val file    = touch(dir / "artifact.jar")
        val res     = fixture.run(dependencies, files = Seq(file))
        assert(res == Right(Seq(file)))
        assert(fixture.resolutionCount == 1)
        assert(fixture.log.contains("resolution-cache not-cacheable"))
        assert(fixture.log.contains(expectedReason))
        assert(entries(dir).isEmpty)
      }

      test("latest version") {
        check(
          Seq(dep("commons-io", "commons-io", "latest.release")),
          ResolutionCache.Params(),
          "non-concrete version 'latest.release'"
        )
      }

      test("version interval") {
        check(
          Seq(dep("commons-io", "commons-io", "[2.11,2.17)")),
          ResolutionCache.Params(),
          "non-concrete version '[2.11,2.17)'"
        )
      }

      test("snapshot version") {
        check(
          Seq(dep("commons-io", "commons-io", "2.17.0-SNAPSHOT")),
          ResolutionCache.Params(),
          "changing version '2.17.0-SNAPSHOT'"
        )
      }

      test("underscore version without automatic version") {
        check(
          Seq(dep("commons-io", "commons-io", "_")),
          ResolutionCache.Params(),
          "no automatic version for commons-io:commons-io"
        )
      }

      test("underscore version with automatic version is cacheable") {
        val dir = os.temp.dir(prefix = "almond-resolution-cache-test")
        val fixture = new Fixture(
          dir,
          ResolutionCache.Params(
            automaticVersions = Map(Module.of("commons-io", "commons-io") -> "2.16.1")
          )
        )
        val file = touch(dir / "artifact.jar")
        assert(fixture.run(Seq(dep("commons-io", "commons-io", "_")), files = Seq(file)).isRight)
        assert(!fixture.log.contains("not-cacheable"))
        assert(entries(dir).length == 1)
      }

      test("snapshot artifact") {
        val dir     = os.temp.dir(prefix = "almond-resolution-cache-test")
        val fixture = new Fixture(dir)
        val file    = touch(dir / "commons-io-1.0-SNAPSHOT.jar")
        assert(fixture.run(
          Seq(dep("commons-io", "commons-io", "2.16.1")),
          files = Seq(file)
        ).isRight)
        assert(fixture.log.contains("resolution-cache not-cacheable"))
        assert(fixture.log.contains("changing artifact"))
        assert(entries(dir).isEmpty)
      }
    }

    test("stale entry") {
      val dir     = os.temp.dir(prefix = "almond-resolution-cache-test")
      val jar     = dir / "artifacts" / "commons-io.jar"
      val fixture = new Fixture(dir)
      val deps    = Seq(dep("commons-io", "commons-io", "2.16.1"))

      assert(fixture.run(deps, files = Seq(touch(jar))).isRight)
      assert(fixture.resolutionCount == 1)
      assert(entries(dir).length == 1)

      os.remove(jar)

      val res = fixture.run(deps, files = Seq(touch(jar)))
      assert(res.isRight)
      assert(fixture.resolutionCount == 2)
      assert(fixture.log.contains("resolution-cache stale"))
      assert(fixture.log.contains(jar.toString))
    }

    test("hit returns the stored files") {
      val dir     = os.temp.dir(prefix = "almond-resolution-cache-test")
      val files   = Seq(touch(dir / "a.jar"), touch(dir / "b.jar"))
      val deps    = Seq(dep("commons-io", "commons-io", "2.16.1"))
      val fixture = new Fixture(dir)

      assert(fixture.run(deps, files = files) == Right(files))
      assert(fixture.run(deps, files = Nil) == Right(files))
      assert(fixture.resolutionCount == 1)
      assert(fixture.log.contains("resolution-cache hit"))
    }

    test("key sensitivity") {
      val dir     = os.temp.dir(prefix = "almond-resolution-cache-test")
      val file    = touch(dir / "artifact.jar")
      val commons = dep("commons-io", "commons-io", "2.16.1")
      val slf4j   = dep("org.slf4j", "slf4j-api", "1.7.36")

      def run(
        dependencies: Seq[Dependency],
        repositories: Seq[Repository] = Seq(central),
        params: ResolutionCache.Params = ResolutionCache.Params()
      ): Unit = {
        val fixture = new Fixture(dir, params)
        assert(fixture.run(dependencies, repositories, Seq(file)).isRight)
      }

      run(Seq(commons, slf4j))
      assert(entries(dir).length == 1)

      // same dependencies, listed the other way round - same entry
      run(Seq(slf4j, commons))
      assert(entries(dir).length == 1)

      run(Seq(dep("commons-io", "commons-io", "2.15.1"), slf4j))
      assert(entries(dir).length == 2)

      run(
        Seq(commons, slf4j),
        repositories = Seq(central, MavenRepository.of("https://jitpack.io"))
      )
      assert(entries(dir).length == 3)

      run(Seq(Dependency.of(commons).addExclusion("org.slf4j", "slf4j-api"), slf4j))
      assert(entries(dir).length == 4)

      run(
        Seq(commons, slf4j),
        params = ResolutionCache.Params(forceMavenProperties = Map("some.prop" -> "value"))
      )
      assert(entries(dir).length == 5)

      run(
        Seq(commons, slf4j),
        params = ResolutionCache.Params(mavenProfiles = Map("some-profile" -> true))
      )
      assert(entries(dir).length == 6)

      // and the very first inputs still hit the entry they wrote
      val fixture = new Fixture(dir)
      assert(fixture.run(Seq(commons, slf4j), Seq(central), Nil) == Right(Seq(file)))
      assert(fixture.resolutionCount == 0)
      assert(entries(dir).length == 6)
    }

    test("no secrets on disk") {
      val dir  = os.temp.dir(prefix = "almond-resolution-cache-test")
      val file = touch(dir / "artifact.jar")
      val deps = Seq(dep("com.example", "lib", "1.0.0"))

      val withCredentials = MavenRepository
        .of("https://user:pa55word@example.com/repo")
        .withCredentials(Credentials.of("user", "t0ken"))

      val fixture = new Fixture(dir)
      assert(fixture.run(deps, Seq(central, withCredentials), Seq(file)).isRight)

      val written = entries(dir)
      assert(written.length == 1)
      val content = os.read(dir / written.head)
      assert(!content.contains("pa55word"))
      assert(!content.contains("t0ken"))
      assert(content.contains("example.com/repo"))
      assert(!fixture.log.contains("pa55word"))
      assert(!fixture.log.contains("t0ken"))

      // credentials don't change what gets resolved, so they don't change the key either
      val plain = new Fixture(dir)
      assert(
        plain.run(deps, Seq(central, MavenRepository.of("https://example.com/repo")), Nil) ==
          Right(Seq(file))
      )
      assert(plain.resolutionCount == 0)
      assert(entries(dir).length == 1)
    }
  }
}
