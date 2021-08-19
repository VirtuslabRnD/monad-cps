import $ivy.`org.typelevel::cats-core:2.6.1`
import cats.implicits._
import scala.continuations.*
import cats.MonadError

class MonadErrorExecutor[F[_], E](using MonadError[F, E]) extends Executor[Any]:
  type Output[R] = F[R]
  type Extract = Any
  type Suspended[X] = F[X]

  def process[TA](sm: Coroutine[TA]): Output[TA] =
    import sm.State.*

    def rec(value: F[(Any, sm.Frame)]): F[Any] = value.flatMap { (v, f) => handle(f.resume(v)) }

    def handle: sm.State => F[Any] =
      case Finished(answer) => MonadError[F, E].point(answer)
      case Failed(e) => throw e
      case p @ Progressed(v, f) =>
        rec(v.map(_ -> f))

    handle(sm.start()).asInstanceOf[F[TA]]

extension [E, F[_], X](e: F[X])(using MonadError[F, E]) inline def extractError: MonadicError[F, E, X] =
  summon[MonadErrorExecutor[F, E]#C].suspend[X](e)

type MonadicError[F[_], E, +R] = MonadError[F, E] ?=>  MonadErrorExecutor[F, E]#C ?=> R
