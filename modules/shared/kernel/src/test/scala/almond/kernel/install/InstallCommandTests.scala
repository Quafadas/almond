package almond.kernel.install

import java.nio.file.{Files, Path}
import java.util.jar.{Attributes, JarOutputStream, Manifest}

import utest._

object InstallCommandTests extends TestSuite {

  /** A JAR whose manifest names a main class, which is all [[Install.currentAppCommand]] reads. */
  private def dummyLauncher(dir: Path): Path = {
    val jar      = dir.resolve("launcher.jar")
    val manifest = new Manifest
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.MAIN_CLASS, "almond.ScalaKernel")
    val os = new JarOutputStream(Files.newOutputStream(jar), manifest)
    os.close()
    jar
  }

  private def withProps[T](props: Seq[(String, String)])(f: => T): T = {
    val previous = props.map { case (k, _) => k -> Option(System.getProperty(k)) }
    try {
      for ((k, v) <- props)
        System.setProperty(k, v)
      f
    }
    finally
      for ((k, valueOpt) <- previous)
        valueOpt match {
          case Some(value) => System.setProperty(k, value)
          case None        => System.clearProperty(k)
        }
  }

  private def launchedWith(jar: Path, args: Seq[String]): Seq[(String, String)] =
    Seq("coursier.mainJar" -> jar.toString) ++
      args.zipWithIndex.map { case (arg, idx) => s"coursier.main.arg-$idx" -> arg }

  val tests = Tests {

    test("kernel options survive --install") {

      val dir = Files.createTempDirectory("almond-install-test")
      val jar = dummyLauncher(dir)

      // what the user typed: install flags, plus options meant for the installed kernel
      val args = Seq("--install", "--force", "--resolution-cache", "--log", "info")

      val commandOpt = withProps(launchedWith(jar, args)) {
        Install.currentAppCommand(
          Nil,
          Set("--install", "--force", "--global").flatMap(s => Seq(s, s"$s=true"))
        )
      }

      val command = commandOpt.getOrElse(sys.error("No command found"))
      assert(command.contains("--resolution-cache"))
      assert(command.contains("--log"))
      assert(command.contains("info"))
      assert(!command.contains("--install"))
      assert(!command.contains("--force"))
    }

    test("kernel options land in kernel.json") {

      val dir         = Files.createTempDirectory("almond-install-test")
      val jar         = dummyLauncher(dir)
      val jupyterPath = Files.createDirectory(dir.resolve("kernels"))

      val args = Seq("--install", "--resolution-cache", "--resolution-cache-dir", "/tmp/rc")

      val installed = withProps(launchedWith(jar, args)) {
        Install.install(
          defaultId = "scala",
          defaultDisplayName = "Scala",
          language = "scala",
          options = Options(
            id = Some("scala-test"),
            jupyterPath = Some(jupyterPath.toString),
            copyLauncher = Some(false)
          )
        )
      }

      val kernelJson =
        new String(Files.readAllBytes(installed.resolve("kernel.json")), "UTF-8")

      assert(kernelJson.contains("--resolution-cache"))
      assert(kernelJson.contains("--resolution-cache-dir"))
      assert(kernelJson.contains("/tmp/rc"))
      assert(!kernelJson.contains("--install"))
    }
  }
}
