import scala.util.Random

def isEven(i: Int): Eval[Boolean] =
  Eval.later(i == 0).flatMap {
    case true  => Eval.now(true)
    case false => isOdd(i - 1)
  }

def isOdd(i: Int): Eval[Boolean] =
  Eval.later(i == 0).flatMap {
    case true  => Eval.now(false)
    case false => isEven(i - 1)
  }

val odd  = Printer.expr(isOdd(100501))
val even = Printer.expr(isEven(100501))

Printer.expr(odd.result)
Printer.expr(even.result)

def hasEvenEval(is: Iterable[Int]): Eval[Boolean] =
  Eval.later(is.isEmpty).flatMap { empty =>
    if empty then Eval.now(false)
    else if is.head % 2 == 0 then Eval.now(true)
    else hasEvenEval(is.tail)
  }

Printer.expr(hasEvenEval(LazyList.continually(Random.nextInt(1024))).result)

def foldRight[A, B](ia: Iterable[A], zero: Eval[B])(
    f: (A, Eval[B]) => Eval[B]
): Eval[B] =
  def loop(ia: Iterable[A]): Eval[B] =
    Eval.later(ia.isEmpty).flatMap {
      case true  => zero
      case false => f(ia.head, loop(ia.tail))
    }

  loop(ia)
end foldRight

def hasEvenFold(is: Iterable[Int]): Eval[Boolean] =
  foldRight(is, Eval.now(false)) { (i, acc) =>
    if i % 2 == 0 then Eval.now(true)
    else acc
  }

Printer.expr(hasEvenFold(LazyList.continually(Random.nextInt(1024))).result)
