import scala.quoted.*

object Printer:
  inline def expr[A](inline expr: => A): A = ${ printExprMacro('expr) }

  private def printExprMacro[A](expr: Expr[A])(using Quotes, Type[A]): Expr[A] =
    val exprShow = Expr(expr.show)
    '{
      val res = $expr
      println($exprShow ++ " = " ++ res.toString)
      res
    }
  end printExprMacro
end Printer
