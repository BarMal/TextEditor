package app.algebra.impl

import app.algebra.ConfigAlgebra
import app.algebra.impl.PureConfigAlgebra.ConfigLoadError
import app.config.AppConfig
import cats.effect.{Resource, Sync}
import cats.syntax.all.*
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderFailures

class PureConfigAlgebra[F[_]: Sync] extends ConfigAlgebra[F] {
  def loadConfig: F[AppConfig] =
    Sync[F].fromEither(
      ConfigSource.defaultApplication
        .load[AppConfig]
        .leftMap(ConfigLoadError(_))
    )
}

object PureConfigAlgebra {
  case class ConfigLoadError(failures: ConfigReaderFailures)
      extends RuntimeException(failures.toString)
}
