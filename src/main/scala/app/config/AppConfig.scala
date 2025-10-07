package app.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

case class WindowConfig(
  width: Int = 120,
  height: Int = 40
) derives ConfigReader

case class AppConfig(
  filePath: String,
  title: Option[String] = None,
  initialSize: Option[WindowConfig] = None
) derives ConfigReader
