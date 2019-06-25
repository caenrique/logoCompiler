package evaluator

import errors.LogoCompilationError.LogoEvaluationError
import parser._

import scala.util.{Left, Random, Right}

object ExpresionEvaluator {

  import evaluator.Value._

  def eval(expr: Expresion, s: SymbolTable)
          (f: Int => EvalData)
  : EvalData = {
    ExpresionEvaluator(expr, s) match {
      case Left(err) => EvalData(Left(err), s)
      case Right(number) => f(number)
    }
  }

  def eval2(expr1: Expresion, expr2: Expresion, s: SymbolTable)
           (f: (Int, Int) => EvalData)
  : EvalData = {
    val ab = for {
      aValue <- ExpresionEvaluator(expr1, s)
      bValue <- ExpresionEvaluator(expr2, s)
    } yield (aValue, bValue)
    ab match {
      case Left(err) => EvalData(Left(err), s)
      case Right((av, bv)) => f(av, bv)
    }
  }

  def eval3(expr1: Expresion, expr2: Expresion, expr3: Expresion, s: SymbolTable)
           (f: (Int, Int, Int) => EvalData)
  : EvalData = {
    val abc = for {
      a <- ExpresionEvaluator(expr1, s)
      b <- ExpresionEvaluator(expr2, s)
      c <- ExpresionEvaluator(expr3, s)
    } yield (a, b, c)
    abc match {
      case Left(err) => EvalData(Left(err), s)
      case Right((a, b, c)) => f(a, b, c)
    }
  }

  def apply(expresion: Expresion, simbolos: SymbolTable): ValueResult = {
    val epResult = expresion.ep.map(epEval(_, simbolos)).getOrElse(identity(_: ValueResult))
    epResult(terminoEval(expresion.termino, simbolos))
  }

  private def nativaEval(nativa: NativaExpr, simbolos: SymbolTable): ValueResult = {
    ExpresionEvaluator(nativa.parametro, simbolos).map { num =>
      nativa.funcion match {
        case "random" => new Random(System.nanoTime()).nextInt(num)
      }
    }
  }

  private def factorEval(factor: Factor, simbolos: SymbolTable): ValueResult = factor match {
    case Numero(n) => Right(n)
    case Variable(n) => simbolos.vars.get(n) match {
      case Some(value) => Right(value)
      case None => Left(LogoEvaluationError(s"la variable $n no está definida"))
    }
    case n: NativaExpr => nativaEval(n, simbolos)
    case ParentesisExpr(expresion) => ExpresionEvaluator(expresion, simbolos)
  }

  private def tpEval(tp: Tp, simbolos: SymbolTable): ResultFunction = {
    def computeLast(f: Factor, op: Operacion) = combineValues(op, _, factorEval(f, simbolos))
    def compute(f: Factor, op: Operacion, s: Tp) = (combineValues(op, _, factorEval(f, simbolos))).andThen(tpEval(s, simbolos))
    tp match {
      case Por(f, None) => computeLast(f, Value.SUMA)
      case Por(f, Some(s)) => compute(f, Value.MULTIPLICACION, s)
      case Dividido(f, None) => computeLast(f, Value.DIVISION)
      case Dividido(f, Some(s)) => compute(f, Value.DIVISION, s)
    }
  }

  private def terminoEval(termino: Termino, simbolos: SymbolTable): ValueResult = {
    val tpResult = termino.tp.map(tpEval(_, simbolos)).getOrElse(identity(_: ValueResult))
    tpResult(factorEval(termino.factor, simbolos))
  }

  private def epEval(ep: Ep, simbolos: SymbolTable): ResultFunction = {
    def computeLast(t: Termino, op: Operacion) = combineValues(op, _, terminoEval(t, simbolos))
    def compute(t: Termino, op: Operacion, s: Ep) = (combineValues(op, _, terminoEval(t, simbolos))).andThen(epEval(s, simbolos))
    ep match {
      case Mas(t, None) => computeLast(t, Value.SUMA)
      case Mas(t, Some(s)) => compute(t, Value.SUMA, s)
      case Menos(t, None) => computeLast(t, Value.RESTA)
      case Menos(t, Some(s)) => compute(t, Value.RESTA, s)
    }
  }

  // TODO: Extraer esta prueba a un test unitario
  def main(args: Array[String]): Unit = {
    val result = ExpresionEvaluator(Expresion(Termino(Variable("hola"), None), Some(Mas(Termino(Numero(6),Some(Dividido(Numero(2),None))),None))), SymbolTable.empty.copy(vars = Map("hola" -> 4)))
    result match {
      case Right(value) => println(s"el resultado es $value")
      case Left(LogoEvaluationError(msg)) => println(msg)
    }
  }

}
