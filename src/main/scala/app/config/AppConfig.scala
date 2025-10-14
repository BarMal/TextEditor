package app.config

import pureconfig.ConfigReader

case class WindowConfig(
    width: Int,
    height: Int
) derives ConfigReader

object WindowConfig {
  val default: WindowConfig = WindowConfig(120, 40)
}

case class AppConfig(
    title: Option[String] = None,
    initialSize: Option[WindowConfig] = None
) derives ConfigReader

object Title {
  val default: String = "BAM Editor"
}
