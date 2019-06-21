package parser

object LogoCompiler {

  def apply(code: String): Either[LogoCompilationError, LogoAST] = {
    for {
      tokens <- LogoLexer(code)
      ast <- LogoParser(tokens)
    } yield ast
  }

}
