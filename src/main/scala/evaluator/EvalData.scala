package evaluator

import errors.LogoCompilationError.LogoEvaluationError
import evaluator.EvalData.EvalResult
import util.Monoid

case class EvalData(result: EvalResult, symbols: SymbolTable)

object EvalData {

  import scala.util.Right

  type EvalResult = Either[LogoEvaluationError, String]

  val empty: EvalData = evaluator.EvalData(Right(""), SymbolTable.empty)

  def withSymbols(s: SymbolTable) = evaluator.EvalData(Right(""), s)

  implicit val evalDataMonoid = new Monoid[EvalData] {
    override def empty: EvalData = EvalData.empty
    override def combine(x: EvalData, y: EvalData): EvalData = {
      val result = for {
        xResult <- x.result
        yResult <- y.result
      } yield xResult + " " + yResult

      EvalData(result, y.symbols)
    }
  }
}
