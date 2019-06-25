package errors

trait LogoCompilationError

object LogoCompilationError {

  case class LogoLexerError(location: Location, msg: String) extends LogoCompilationError
  case class LogoParserError(location: Location, msg: String) extends LogoCompilationError
  case class LogoEvaluationError(msg: String) extends LogoCompilationError

  case class Location(linea: Int, columna: Int) {
    override def toString: String = s"$linea:$columna"
  }

}
