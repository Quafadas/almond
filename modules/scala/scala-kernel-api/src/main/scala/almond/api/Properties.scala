package almond.api

import java.util.{Properties => JProperties}

object Properties {

  private lazy val props = {

    val p = new JProperties

    try
      p.load(
        getClass
          .getClassLoader
          .getResourceAsStream("almond/almond.properties")
      )
    catch {
      case _: NullPointerException =>
    }

    p
  }

  lazy val version    = Option(props.getProperty("version")).getOrElse("[unknown]")
  lazy val commitHash = Option(props.getProperty("commit-hash")).getOrElse("[unknown]")

  lazy val ammoniteSparkVersion =
    Option(props.getProperty("ammonite-spark-version")).getOrElse("[unknown]")
  lazy val defaultScalafmtVersionOpt = Option(props.getProperty("default-scalafmt-version"))
  lazy val defaultScalafmtVersionJava8Opt =
    Option(props.getProperty("default-scalafmt-version-java-8"))

  def defaultScalafmtVersion(javaVersion: String): Option[String] =
    if (javaVersion.startsWith("1.")) defaultScalafmtVersionJava8Opt
    else defaultScalafmtVersionOpt
  def defaultScalafmtVersion(): Option[String] =
    defaultScalafmtVersion(sys.props("java.version"))

  lazy val actualScalaVersion = Option(props.getProperty("scala-version")).getOrElse("[unknown]")

}
