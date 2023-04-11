import cats.*
import cats.data.*
import cats.syntax.all.*

def squared[F[_]: Applicative](a: Double): F[Double] =
  math.pow(a, 2.0).pure[F]

def positive[F[_]: ApplicativeThrow](a: Double): F[Unit] =
  ApplicativeThrow[F].raiseWhen(a <= 0) {
    java.lang.IllegalArgumentException(s"Non positive value: $a")
  }

def pythMonad[F[_]: Monad](a: Double, b: Double): F[Double] = for
  a2 <- squared[F](a)
  b2 <- squared[F](b)
yield math.sqrt(a2 + b2)

Printer.expr(pythMonad[Option](3.0, 4.0))

def pythMonadChecked[F[_]: MonadThrow](a: Double, b: Double): F[Double] = for
  _ <- positive[F](a)
  _ <- positive[F](b)
  c <- pythMonad[F](a, b)
yield c

Printer.expr(pythMonadChecked[Either[Throwable, _]](3.0, 4.0))
Printer.expr(pythMonadChecked[Either[Throwable, _]](-3.0, 4.0))

def pythApplicative[F[_]: Applicative](
    a: Double,
    b: Double
): F[Double] = (squared[F](a), squared[F](b)).mapN { (a2, b2) =>
  math.sqrt(a2 + b2)
}

Printer.expr(pythApplicative[Option](3.0, 4.0))

def pythApplicativeChecked(
    a: Double,
    b: Double
): Validated[NonEmptyChain[Throwable], Double] = (
  positive[Either[Throwable, _]](a).toValidatedNec,
  positive[Either[Throwable, _]](b).toValidatedNec,
).tupled *> pythApplicative(a, b)

Printer.expr(pythApplicativeChecked(3.0, 4.0))
Printer.expr(pythApplicativeChecked(-3.0, -4.0))
