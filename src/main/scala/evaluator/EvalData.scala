package evaluator

import errors.LogoCompilationError.LogoEvaluationError
import evaluator.EvalData.EvalResult
import util.Monoid
import SymbolTable._

import scala.util.Right

case class EvalData(result: EvalResult, symbols: SymbolTable)

object EvalData {

  type EvalResult = Either[LogoEvaluationError, List[String]]

  val empty: EvalData = EvalData(Right(Nil), SymbolTable.empty)

  def withSymbols(s: SymbolTable) = EvalData(Right(Nil), s)

  implicit class EvalDataOps(evd: EvalData) {
    def incVar(varName: String, inc: Int): EvalData = {
      evd.symbols.vars.get(varName) match {
        case None => evd.copy(symbols = evd.symbols + (varName, inc))
        case Some(value) => evd.copy(symbols = evd.symbols + (varName, value + inc))
      }
    }
    def replaceSymbols(s: SymbolTable): EvalData = evd.copy(symbols = s)
  }

  implicit val evalDataMonoid: Monoid[EvalData] = new Monoid[EvalData] {
    override def empty: EvalData = EvalData.empty

    override def combine(x: EvalData, y: EvalData): EvalData = {
      val result = for {
        xResult <- x.result
        yResult <- y.result
      } yield xResult ++ yResult

      EvalData(result, y.symbols)
    }
  }
}
