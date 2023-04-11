def squared(a: Double): Option[Double] =
  Option.when(a > 0)(math.pow(a, 2.0))

def pythMatch(a: Double, b: Double): Option[Double] =
  squared(a) match
    case None => None
    case Some(a) =>
      squared(b) match
        case None    => None
        case Some(b) => Some(math.sqrt(a + b))
end pythMatch

Printer.expr(pythMatch(3.0, 4.0))

def pythFlatMap(a: Double, b: Double): Option[Double] =
  squared(a).flatMap { a =>
    squared(b).flatMap { b =>
      Some(math.sqrt(a + b))
    }
  }

Printer.expr(pythFlatMap(3.0, 4.0))

def pythFor(a: Double, b: Double): Option[Double] = for
  a <- squared(a)
  b <- squared(b)
yield math.sqrt(a + b)

Printer.expr(pythFor(3.0, 4.0))

def pythTuple(a: Double, b: Double): Option[Double] =
  (squared(a), squared(b)) match
    case (Some(a), Some(b)) => Some(math.sqrt(a + b))
    case _                  => None

Printer.expr(pythTuple(3.0, 4.0))
