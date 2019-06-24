package parser

import parser.EvalData.EvalResult
import parser.LogoCompilationError.LogoEvaluationError

case class EvalData(result: EvalResult, symbols: SymbolTable)

object EvalData {

  import scala.util.Right

  type EvalResult = Either[LogoEvaluationError, String]

  val empty: EvalData = EvalData(Right(""), SymbolTable.empty)

  def withSymbols(s: SymbolTable) = EvalData(Right(""), s)

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
