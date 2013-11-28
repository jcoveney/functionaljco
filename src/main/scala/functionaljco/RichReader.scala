package functionaljco

import com.twitter.algebird.monad.{ Reader => AlgReader }
import com.twitter.bijection._

object RichReader {
  implicit def norm2richBij[Env, T] = new Bijection[Reader[Env, T], RichReader[Env, T]] {
    override def apply(r: Reader[Env, T]) = new RichReader(r)
    override def invert(r: RichReader[Env, T]) = r.r
  }
  //TODO is there a way to make it so the bijection is in scope and not have these?
  implicit def norm2rich[Env, T](r: Reader[Env, T]) = norm2richBij(r)
  implicit def rich2norm[Env, T](r: RichReader[Env, T]) = norm2richBij.invert(r)

  def apply[Env, T](f: Env => T) : RichReader[Env, T] = Reader(f)
}
class RichReader[-Env, +T](val r: Reader[Env, T]) extends Reader[Env, T] {
  def apply(e: Env) : T = r(e)

  def join[Env2 <: Env, U](that: Reader[Env2, U]) : Reader[Env2, (T, U)] =
    Reader { e => (this(e), that(e)) }

  def concat[Env2, U](that: Reader[Env2, U]) : Reader[(Env, Env2), (T, U)] =
    Reader { e =>
      val (e1, e2) = e
      (this(e1), that(e2))
    }

  def comap[NewEnv](f: NewEnv => Env) : Reader[NewEnv, T] =
    Reader { e => this(f(e)) }
}
object Reader {
  implicit def norm2algBij[Env, T] = new Bijection[Reader[Env, T], AlgReader[Env, T]] {
    override def apply(r: Reader[Env, T]) = AlgReader { e => r(e) }
    override def invert(r: AlgReader[Env, T]) = Reader { e => r(e) }
  }

  implicit def norm2alg[Env, T](r: Reader[Env, T]) = norm2algBij(r)
  implicit def alg2norm[Env, T](r: AlgReader[Env, T]) : Reader[Env, T] = norm2algBij.invert(r)

  def apply[Env, T](f: Env => T) : Reader[Env, T] = new Reader[Env, T] {
    def apply(e: Env) = f(e)
  }
}
//TODo would love to use algebird but we want RichReader to be a Reader
trait Reader[-Env, +T] extends (Env => T) {
  def apply(e: Env) : T
  def map[U](f: T => U) : Reader[Env, U] = Reader { e => f(this(e)) }
  def flatMap[NewEnv <: Env, U](f: T => Reader[NewEnv, U]) : Reader[NewEnv, U] =
    Reader { e => f(this(e))(e) }
}
