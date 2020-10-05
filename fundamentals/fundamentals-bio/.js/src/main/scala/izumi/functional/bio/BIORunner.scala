package izumi.functional.bio

/** Scala.js does not support BIORunner */
trait BIORunner[F[_, _]]

object BIORunner {
  @inline def apply[F[_, _]](implicit ev: UnsafeRun2[F]): UnsafeRun2[F] = ev

  implicit def anyBIORunner[F[_, _]]: UnsafeRun2[F] = new UnsafeRun2[F] {}
}