package compiler

import errors.LogoCompilationError
import evaluator.{LogoCodeFormater, LogoCodeGenerator}
import parser.{LogoLexer, LogoParser}

object LogoCompiler {

  def apply(code: String): Either[LogoCompilationError, String] = {
    for {
      tokens <- LogoLexer(code)
      ast <- LogoParser(tokens)
      instructions <- LogoCodeGenerator(ast)
    } yield LogoCodeFormater(instructions)
  }

}
