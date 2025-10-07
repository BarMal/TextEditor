package app.algebra

import app.config.AppConfig
import cats.MonadError

/**
 * Algebra for configuration operations
 */
trait ConfigAlgebra[F[_]] {
  def loadConfig: F[AppConfig]
  def getEditorTitle: F[String]
  def getInitialSize: F[Option[(Int, Int)]]
}
