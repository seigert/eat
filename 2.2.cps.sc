import scala.util.Random

def pow2[R](a: Double): (Double => R) => R           = next => next(math.pow(a, 2.0))
def sqrt[R](a: Double): (Double => R) => R           = next => next(math.sqrt(a))
def add[R](a: Double, b: Double): (Double => R) => R = next => next(a + b)

def pyth[R](a: Double, b: Double): (Double => R) => R = next =>
  pow2(a) { a2 =>
    pow2(b) { b2 =>
      add(a2, b2) { anb =>
        sqrt(anb)(next)
      }
    }
  }

Printer.expr(pyth(3.0, 4.0)(identity))

type Cont[R, A] = (A => R) => R
object Cont:
  def apply[R, A](f: (A => R) => R): Cont[R, A] = f
  def pure[R, A](a: A): Cont[R, A]              = apply(cb => cb(a))

  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] = apply { cb =>
    val cont = f(a => apply(_ => cb(a)))
    cont(cb)
  }
end Cont

extension [R, A](cont: Cont[R, A])
  def map[B](f: A => B): Cont[R, B] = Cont { cb =>
    cont(f.andThen(cb))
  }

  def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = Cont { cb =>
    val run: Cont[R, B] => R = c => c(cb)
    cont(f.andThen(run))
  }
end extension

def contPow2[R](a: Double): Cont[R, Double]           = cb => cb(math.pow(a, 2.0))
def contSqrt[R](a: Double): Cont[R, Double]           = cb => cb(math.sqrt(a))
def contAdd[R](a: Double, b: Double): Cont[R, Double] = cb => cb(a + b)

def contPyth[R](a: Double, b: Double): Cont[R, Double] = for
  a2  <- contPow2(a)
  b2  <- contPow2(b)
  anb <- contAdd(a2, b2)
  c   <- contSqrt(anb)
yield c

Printer.expr(contPyth(3.0, 4.0)(identity))

def contPyth2[R](a: Double, b: Double): Cont[R, Option[Double]] =
  Cont.callCC((exit: Option[Double] => Cont[R, Option[Double]]) =>
    if a <= 0 || b <= 0 then exit(None)
    else contPyth(a, b).map(Some(_))
  )

Printer.expr(contPyth2(-3.0, 4.0)(identity))
Printer.expr(contPyth2(3.0, 4.0)(identity))

def hasEven[R](is: Iterable[Int]): Cont[R, Boolean] =
  Cont.callCC { (exit: Boolean => Cont[R, Boolean]) =>
    if is.isEmpty then exit(false)
    else if is.head % 2 == 0 then exit(true)
    else hasEven(is.tail)
  }

Printer.expr(hasEven(Some(1))(identity))
Printer.expr(hasEven(List(1, 3, 5, 20))(identity))
Printer.expr(hasEven(LazyList.continually(Random.nextInt(1024)))(identity))
