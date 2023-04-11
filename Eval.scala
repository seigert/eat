import scala.util.control.NonFatal
import scala.annotation.tailrec

sealed trait Eval[A]:
  self =>

  import Eval.*

  def result: Either[Throwable, A]

  def value: A = result match
    case Right(value) => value
    case Left(err)    => throw err

  def map[B](f: A => B): Eval[B] = flatMap(a => later(f(a)))
  def flatMap[B](f: A => Eval[B]): Eval[B] = self match
    case fm: FlatMap[A] =>
      new FlatMap[B]:
        type Head = fm.Head

        val head = fm.head
        val tail = hd =>
          new FlatMap[B]:
            type Head = A
            val head = () => fm.tail(hd)
            val tail = _.fold(fail, f)

    case _ =>
      new FlatMap[B]:
        type Head = A
        val head = () => self
        val tail = _.fold(fail, f)
  end flatMap

  override def toString(): String = self match
    case n: Now[?]     => s"Eval.Now(${n.value})"
    case f: Failure[?] => s"Eval.Failure(${f.err})"
    case _: Later[?]   => s"Eval.Later(..)"
    case _: FlatMap[?] => s"Eval.FlatMap(.., .. => ..)"

end Eval

object Eval:
  def now[A](a: A): Eval[A]            = Now(a)
  def later[A](thunk: => A): Eval[A]   = Later(() => thunk)
  def fail[A](err: Throwable): Eval[A] = Failure(err)

  private[Eval] final class Now[A](override val value: A) extends Eval[A]:
    def result = Right(value)

  private[Eval] final class Failure[A](val err: Throwable) extends Eval[A]:
    def result = Left(err)

  private[Eval] final class Later[A](thunk: () => A) extends Eval[A]:
    def result: Either[Throwable, A] =
      try Right(thunk())
      catch case NonFatal(err) => Left(err)
  end Later

  private[Eval] abstract class FlatMap[A] extends Eval[A]:
    type Head

    val head: () => Eval[Head]
    val tail: Either[Throwable, Head] => Eval[A]

    def result: Either[Throwable, A] = evaluate(this)
  end FlatMap

  private[Eval] sealed trait Stack[A, B]
  private[Eval] final class One[A, B](val f: A => B) extends Stack[A, B]
  private[Eval] final class Many[A, B, C](
      val head: Either[Throwable, A] => Eval[B],
      val tail: Stack[B, C]
  ) extends Stack[A, C]

  private[Eval] def evaluate[A](eval: Eval[A]): Either[Throwable, A] =
    @tailrec def loop[A1](eval: Eval[A1], stack: Stack[A1, A]): Either[Throwable, A] = eval match
      case f: Failure[A1] => Left(f.err)
      case fm: FlatMap[A1] =>
        fm.head() match
          case f: Failure[fm.Head] => Left(f.err)
          case fm1: FlatMap[fm.Head] => loop(fm1.head(), Many(fm1.tail, Many(fm.tail, stack)))
          case inner                 => loop(fm.tail(inner.result), stack)

      case _ =>
        stack match
          case o: One[A1, A]     => eval.result.map(o.f)
          case m: Many[A1, b, A] => loop(m.head(eval.result), m.tail)

    loop(eval, One(identity))
  end evaluate

end Eval
