package app.config

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

case class AppConfig()

object AppConfig {
  given yamlDecoder: YamlDecoder[AppConfig] = YamlDecoder.derived[AppConfig]
}
