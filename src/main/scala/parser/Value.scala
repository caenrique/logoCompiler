package parser

import parser.LogoCompilationError.LogoEvaluationError


object Value {

  import scala.util.Left
  import scala.util.Right

  type Value = Either[Int, String]

  type ValueResult = Either[LogoEvaluationError, Value]

  type Operacion = Value => Value => ValueResult
  type ResultFunction = ValueResult => ValueResult

  def SUMA: Operacion = (a: Value) => (b: Value) => a + b
  def RESTA: Operacion = (a: Value) => (b: Value) => a - b
  def MULTIPLICACION: Operacion = (a: Value) => (b: Value) => a * b
  def DIVISION: Operacion = (a: Value) => (b: Value) => a / b

  def combineValues(op: Operacion, a: ValueResult, b: ValueResult): ValueResult = {
      for {
        xValue <- a
        yValue <- b
        result <- op(xValue)(yValue)
      } yield result
  }

  implicit class OperationsOnValues(a: Value) {
    def +(b: Value): ValueResult = (a, b) match {
      case (Left(an), Left(bn)) => Right(Left(an + bn))
      case (Right(an), Right(bn)) => Right(Right(an + bn))
      case _ => Left(LogoEvaluationError("No se pueden sumar una cadena y un número"))
    }

    def -(b: Value): ValueResult = (a, b) match {
      case (Left(an), Left(bn)) => Right(Left(an - bn))
      case (Right(an), Right(bn)) => Left(LogoEvaluationError("No se pueden restar dos cadenas de texto"))
      case _ => Left(LogoEvaluationError("No se pueden restar una cadena y un número"))
    }

    def *(b: Value): ValueResult = (a, b) match {
      case (Left(an), Left(bn)) => Right(Left(an * bn))
      case (Right(an), Right(bn)) => Left(LogoEvaluationError("No se pueden multiplicar dos cadenas de texto"))
      case _ => Left(LogoEvaluationError("No se pueden multiplicar una cadena y un número"))
    }

    def /(b: Value): ValueResult = (a, b) match {
      case (Left(an), Left(bn)) => Right(Left(an * bn))
      case (Right(an), Right(bn)) => Left(LogoEvaluationError("No se pueden dividir dos cadenas de texto"))
      case _ => Left(LogoEvaluationError("No se pueden dividir una cadena y un número"))
    }
  }

}
