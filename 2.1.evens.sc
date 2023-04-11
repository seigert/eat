import scala.annotation.tailrec
import scala.util.Random

def hasEvenImperative(is: Iterable[Int]): Boolean =
  val iter = is.iterator
  while iter.hasNext do if iter.next % 2 == 0 then return true
  false

Printer.expr(hasEvenImperative(Some(1)))
Printer.expr(hasEvenImperative(List(1, 3, 5, 20)))
Printer.expr(hasEvenImperative(LazyList.continually(Random.nextInt(1024))))

def hasEvenFL(is: Iterable[Int]): Boolean =
  is.foldLeft(false)((acc, i) => acc || (i % 2 == 0))

Printer.expr(hasEvenFL(Some(1)))
Printer.expr(hasEvenFL(List(1, 3, 5, 20)))
// Printer.expr(hasEvenFL(LazyList.continually(Random.nextInt(1024))))

def hasEvenFR(is: Iterable[Int]): Boolean =
  is.foldRight(false)((i, acc) => acc || (i % 2 == 0))

Printer.expr(hasEvenFR(Some(1)))
Printer.expr(hasEvenFR(List(1, 3, 5, 20)))
// Printer.expr(hasEvenFR(LazyList.continually(Random.nextInt(1024))))

def hasEvenTailRec(is: Iterable[Int]): Boolean =
  @tailrec def loop(rest: Iterable[Int]): Boolean =
    if rest.isEmpty then false
    else if rest.head % 2 == 0 then true
    else loop(rest.tail)

  loop(is)
end hasEvenTailRec

Printer.expr(hasEvenTailRec(Some(1)))
Printer.expr(hasEvenTailRec(List(1, 3, 5, 20)))
Printer.expr(hasEvenTailRec(LazyList.continually(Random.nextInt(1024))))
