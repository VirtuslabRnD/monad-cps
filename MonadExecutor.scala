import $ivy.`org.typelevel::cats-core:2.6.1`
import $ivy.`org.typelevel::cats-effect:3.2.3`
import cats.Monad
import cats.implicits._
import scala.continuations.*

class MonadExecutor[F[_]: Monad] extends Executor[Any]:
  type Output[R] = F[R]
  type Extract = Any
  type Suspended[X] = F[X]

  def process[TA](sm: Coroutine[TA]): Output[TA] =
    import sm.State.*

    def rec(value: F[(Any, sm.Frame)]): F[Any] = value.flatMap { (v, f) => handle(f.resume(v)) }

    def handle: sm.State => F[Any] =
      case Finished(answer) => Monad[F].point(answer)
      case Failed(e) => throw e
      case p @ Progressed(v, f) =>
        rec(v.map(_ -> f))

    handle(sm.start()).asInstanceOf[F[TA]]

extension [F[_]: Monad, X](e: F[X]) inline def extract: Monadic[F, X] =
  summon[MonadExecutor[F]#C].suspend[X](e)

infix type Monadic[F[_], +R] = Monad[F] ?=>  MonadExecutor[F]#C ?=> R
