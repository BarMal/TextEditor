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
    filePath: String,
    title: Option[Title] = None,
    initialSize: Option[WindowConfig] = None
) derives ConfigReader

opaque type Title = String
object Title {
  val default: Title = "BAM Editor"
}
