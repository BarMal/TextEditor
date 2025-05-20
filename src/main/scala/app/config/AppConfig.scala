package app.config

import pureconfig.ConfigReader

case class AppConfig(filePath: String) derives ConfigReader
