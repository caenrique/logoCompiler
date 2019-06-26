package evaluator

import errors.LogoCompilationError.LogoEvaluationError
import evaluator.EvalData.EvalResult
import util.Monoid
import SymbolTable._

import scala.util.Right

case class EvalData(result: EvalResult, symbols: SymbolTable)

object EvalData {

  type EvalResult = Either[LogoEvaluationError, String]

  val empty: EvalData = EvalData(Right(""), SymbolTable.empty)

  def withSymbols(s: SymbolTable) = EvalData(Right(""), s)

  implicit class EvalDataOps(evd: EvalData) {
    def incVar(varName: String, inc: Int): EvalData = {
      evd.symbols.vars.get(varName) match {
        case None => evd.copy(symbols = evd.symbols + (varName, inc))
        case Some(value) => evd.copy(symbols = evd.symbols + (varName, value + inc))
      }
    }
    def replaceVarsProcs(s: SymbolTable): EvalData = evd.copy(symbols = evd.symbols.replaceVars(s.vars).replaceProcs(s.proc))
  }

  implicit val evalDataMonoid: Monoid[EvalData] = new Monoid[EvalData] {
    override def empty: EvalData = EvalData.empty

    override def combine(x: EvalData, y: EvalData): EvalData = {
      val result = for {
        xResult <- x.result
        yResult <- y.result
      } yield {
        if (xResult != "" && yResult != "") xResult + ";" + yResult
        else if (xResult != "" && yResult == "") xResult
        else if (xResult == "" && yResult != "") yResult
        else ""
      }

      EvalData(result, y.symbols)
    }
  }
}
