package parser

import Monoid._
import parser.EvalData.EvalResult
import parser.LogoCompilationError.LogoEvaluationError
import parser.Value.ValueResult

import scala.util.Random

object LogoCodeGenerator {

  import scala.util.{Right, Left}
  import SymbolTable._
  // registrar la posicion de la tortuga y el estado del pen
  // Asumo que no existen ámbitos. Todas las variables son globales y se pueden leer si están definidas previamente.
  // Una vez que se definen, permanecen definidas hasta el final de la ejecución

  def apply(ast: LogoAST): EvalResult = ast match {
    case Programa(proc, instr) =>
      val procs = registraProcedimientos(proc)
      val startEvalData = EvalData(Right(""), SymbolTable.empty.withProcs(procs))

      instruccionesEval(instr, startEvalData).result
  }

  def registraProcedimientos(value: List[Procedimiento]): Map[String, Procedimiento] = ???

  def instruccionesEval(instrucciones: List[Instruccion], startEvalData: EvalData): EvalData = {
    instrucciones.foldLeft(startEvalData) {
      case (evalData, nextInstr) => evalData + instruccionEval(nextInstr, evalData.symbols)
    }
  }

  def instruccionEval(instruccion: Instruccion, simbolos: SymbolTable): EvalData = instruccion match {
    case m: Mover => moverEval(m, simbolos)
    case c: Comando => comandoEval(c, simbolos)
    case b: Bucles => buclesEval(b, simbolos)
    case p: LlamadaProcedimiento => ???
  }

  def moverEval(mover: Mover, simbolos: SymbolTable): EvalData = ???

  def comandoEval(comando: Comando, simbolos: SymbolTable): EvalData = comando match {
    case ClearScreen => EvalData(Right("clearScreen"), simbolos)
    case PenDown => EvalData.withSymbols(simbolos.penDown)
    case PenUp => EvalData.withSymbols(simbolos.penUp)
    case HideTurtle => ???
    case ShowTurtle => ???
    case Stop => ???
    case Make(varName, expr) => ExpresionEvaluator(expr, simbolos) match {
      case Left(err) => EvalData(Left(err), simbolos)
      case Right(value) => EvalData.withSymbols(simbolos + (varName, value))
    }
  }

  def buclesEval(bucles: Bucles, table: SymbolTable): EvalData = {
    def forEval(forLoop: For, simbolos: SymbolTable): EvalData = ???
    def repeatEval(repeat: Repeat, simbolos: SymbolTable): EvalData = ???
    def ifEval(ifClause: If, simbolos: SymbolTable): EvalData = ???
    ???
  }

  def bloqueEval(bloque: Bloque, simbolos: SymbolTable): EvalData = {
    instruccionesEval(bloque.instrucciones, EvalData(Right(""), simbolos))
  }

}
