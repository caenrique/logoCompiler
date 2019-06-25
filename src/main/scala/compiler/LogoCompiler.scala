package compiler

import errors.LogoCompilationError
import evaluator.LogoCodeGenerator
import parser.{LogoLexer, LogoParser}

object LogoCompiler {

  def apply(code: String): Either[LogoCompilationError, String] = {
    for {
      tokens <- LogoLexer(code)
      ast <- LogoParser(tokens)
      code <- LogoCodeGenerator(ast)
    } yield code
  }

}
