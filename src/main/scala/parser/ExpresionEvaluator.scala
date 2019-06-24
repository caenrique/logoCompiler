package parser

import parser.LogoCompilationError.LogoEvaluationError
import scala.util.{Left,Right}

import scala.util.Random

object ExpresionEvaluator {

  import Value._

  // TODO: Extraer esta prueba a un test unitario
  def main(args: Array[String]): Unit = {
    val result = ExpresionEvaluator(Expresion(Termino(Variable("hola"), None), Some(Mas(Termino(NativaExpr("random", Expresion(Termino(Numero(5),None),None)), None), None))), SymbolTable.empty.copy(vars = Map("hola" -> Left(4))))
    result match {
      case Right(value) => value match {
        case Left(numero) => println(s"el resultado es $numero")
        case Right(cadena) => println(s"el resultado es $cadena")
      }
      case Left(LogoEvaluationError(msg)) => println(msg)
    }
  }

  def apply(expresion: Expresion, simbolos: SymbolTable): ValueResult = {
    val epResult = expresion.ep.map(epEval(_, simbolos)).getOrElse(identity(_: ValueResult))
    epResult(terminoEval(expresion.termino, simbolos))
  }

  def nativaEval(nativa: NativaExpr, simbolos: SymbolTable): ValueResult = {
    val exprValue: ValueResult = ExpresionEvaluator(nativa.parametro, simbolos)

    exprValue match {
      case Right(Left(num)) => nativa.funcion match {
        case "random" => Right(Left(new Random(System.nanoTime()).nextInt(num)))
      }
      case Right(_) => Left(LogoEvaluationError("No se puede calcular una expresion nativa con una cadena como parametro"))
      case _ => exprValue
    }
  }

  def factorEval(factor: Factor, simbolos: SymbolTable): ValueResult = factor match {
    case Numero(n) => Right(Left(n))
    case Variable(n) => simbolos.vars.get(n) match {
      case Some(value) => Right(value)
      case None => Left(LogoEvaluationError(s"la variable $n no está definida"))
    }
    case Cadena(c) => Right(Right(c))
    case n: NativaExpr => nativaEval(n, simbolos)
    case ParentesisExpr(expresion) => ExpresionEvaluator(expresion, simbolos)
  }

  def tpEval(tp: Tp, simbolos: SymbolTable): ResultFunction = {
    def computeLast(f: Factor, op: Operacion) = combineValues(op, _, factorEval(f, simbolos))
    def compute(f: Factor, op: Operacion, s: Tp) = (combineValues(op, _, factorEval(f, simbolos))).andThen(tpEval(s, simbolos))
    tp match {
      case Por(f, None) => computeLast(f, Value.MULTIPLICACION)
      case Por(f, Some(s)) => compute(f, Value.MULTIPLICACION, s)
      case Dividido(f, None) => computeLast(f, Value.DIVISION)
      case Dividido(f, Some(s)) => compute(f, Value.DIVISION, s)
    }
  }

  def terminoEval(termino: Termino, simbolos: SymbolTable): ValueResult = {
    val tpResult = termino.tp.map(tpEval(_, simbolos)).getOrElse(identity(_: ValueResult))
    tpResult(factorEval(termino.factor, simbolos))
  }

  def epEval(ep: Ep, simbolos: SymbolTable): ResultFunction = {
    def computeLast(t: Termino, op: Operacion) = combineValues(op, _, terminoEval(t, simbolos))
    def compute(t: Termino, op: Operacion, s: Ep) = (combineValues(op, _, terminoEval(t, simbolos))).andThen(epEval(s, simbolos))
    ep match {
      case Mas(t, None) => computeLast(t, Value.SUMA)
      case Mas(t, Some(s)) => compute(t, Value.SUMA, s)
      case Menos(t, None) => computeLast(t, Value.RESTA)
      case Menos(t, Some(s)) => compute(t, Value.RESTA, s)
    }
  }
}