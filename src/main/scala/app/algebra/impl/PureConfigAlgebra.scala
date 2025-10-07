package app.algebra.impl

import app.algebra.ConfigAlgebra
import app.config.AppConfig
import cats.effect.Sync
import cats.syntax.all.*
import pureconfig.ConfigSource

class PureConfigAlgebra[F[_]: Sync] extends ConfigAlgebra[F] {
  def loadConfig: F[AppConfig] =
    Sync[F].delay(ConfigSource.default.loadOrThrow[AppConfig])

  def getEditorTitle: F[String] =
    loadConfig.map(_.title.getOrElse("Bam Editor"))

  def getInitialSize: F[Option[(Int, Int)]] =
    loadConfig.map(cfg => cfg.initialSize.map(s => (s.width, s.height)))
}
