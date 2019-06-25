package evaluator

import errors.LogoCompilationError.LogoEvaluationError

object Value {

  type ValueResult = Either[LogoEvaluationError, Int]

  type Operacion = Int => Int => Int
  type ResultFunction = ValueResult => ValueResult

  val SUMA = (a: Int) => (b: Int) => a + b
  val RESTA = (a: Int) => (b: Int) => a - b
  val MULTIPLICACION = (a: Int) => (b: Int) => a * b
  val DIVISION = (a: Int) => (b: Int) => a / b

  def combineValues(op: Operacion, a: ValueResult, b: ValueResult): ValueResult = {
    for {
      xValue <- a
      yValue <- b
    } yield op(xValue)(yValue)
  }

}
