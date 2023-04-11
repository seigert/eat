def pyth(a: Double, b: Double): Double =
  require(a > 0)
  require(b > 0)

  var result = 0.0
  result += math.pow(a, 2.0)
  result += math.pow(b, 2.0)

  math.sqrt(result)

Printer.expr(pyth(3.0, 4.0))
