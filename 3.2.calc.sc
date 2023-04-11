/** Recursive descent Int calculator.def
  *
  * Grammar:
  *
  * ```
  * expression
  *     + terms
  *     - terms
  *     terms
  *
  * terms
  *     term + terms
  *     term - terms
  *     term
  * term
  *     factor
  *     factor * term
  *     factor / term
  *
  * factor
  *     primary
  *     primary ^ factor
  *
  * primary
  *     ( expression )
  *     number
  * ```
  */
object Calc:

  final case class Parsed(result: Int, leftovers: List[String])

  def apply(s: String): Eval[Int] =
    tokenize(s)
      .flatMap(expression)
      .flatMap {
        case Parsed(result, Nil) => Eval.now(result)
        case Parsed(result, leftovers) =>
          Eval.fail(
            new IllegalArgumentException(
              s"Leftover tokens for '$result': [${leftovers.mkString(" ")}]"
            )
          )
      }

  def tokenize(s: String): Eval[List[String]] =
    Eval.later(s.split(" ").toList)

  def expression(tokens: List[String]): Eval[Parsed] = tokens match
    case "-" :: tail => terms(tail).map(p => p.copy(result = -p.result))
    case "+" :: tail => terms(tail)
    case _           => terms(tokens)

  def terms(tokens: List[String]): Eval[Parsed] = term(tokens).flatMap {
    case Parsed(r, "+" :: tail) => terms(tail).map(p => p.copy(result = r + p.result))
    case Parsed(r, "-" :: tail) => terms(tail).map(p => p.copy(result = r - p.result))
    case p                      => Eval.now(p)
  }

  def term(tokens: List[String]): Eval[Parsed] = factor(tokens).flatMap {
    case Parsed(r, "*" :: tail) => term(tail).map(p => p.copy(result = r * p.result))
    case Parsed(r, "/" :: tail) => term(tail).map(p => p.copy(result = r / p.result))
    case p                      => Eval.now(p)
  }

  def factor(tokens: List[String]): Eval[Parsed] = primary(tokens).flatMap {
    case Parsed(r, "^" :: tail) => term(tail).map(p => p.copy(result = math.pow(r, p.result).toInt))
    case p                      => Eval.now(p)
  }

  def primary(tokens: List[String]): Eval[Parsed] = tokens match
    case "(" :: tail =>
      expression(tail).flatMap {
        case Parsed(r, ")" :: tail) => Eval.now(Parsed(r, tail))
        case _ => Eval.fail(new IllegalArgumentException(s"Unmatched braces in: [${tokens.mkString(" ")}]"))
      }
    case head :: tail => Eval.later(Parsed(head.toInt, tail))
    case _ => Eval.fail(new IllegalArgumentException(s"Unable to parse primary from: [${tokens.mkString(" ")}]"))

end Calc

Printer.expr(Calc("1").result)
Printer.expr(Calc("1 + 1").result)
Printer.expr(Calc("2 * 2").result)
Printer.expr(Calc("( 1 + 3 ) * 4 - ( 6 / ( 3 ^ 0 + 1 ) )").result)
Printer.expr(Calc("( 1 + 3 ) * 4 - ( 6 / ( 3 ^ 0 + 1 )").result)
Printer.expr(Calc("( 1 + 3 ) * 4 ( 6 / ( 3 ^ 0 + 1 ) )").result)
