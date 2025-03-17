package app.config

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

case class AppConfig()

object AppConfig {
  implicit val yamlDecoder: YamlDecoder[AppConfig] = YamlDecoder.derived[AppConfig]
}
