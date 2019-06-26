package parser

import errors.LogoCompilationError.{Location, LogoLexerError}

import scala.util.parsing.combinator.RegexParsers

object LogoLexer extends RegexParsers {
  override def skipWhitespace: Boolean = true
  override val whiteSpace = "(\\s|;.*)+".r

  def numero: Parser[NUMERO] = positioned { "[0-9]+".r ^^ { str => NUMERO(str.toInt) } }
  def variable: Parser[VARIABLE] = positioned { ":[a-zA-Z][a-zA-Z0-9]*".r ^^ { str => VARIABLE(str.substring(1, str.length).toUpperCase) } }
  def cadena: Parser[CADENA] = positioned { """"[a-zA-Z][a-zA-Z0-9]*""".r ^^ { str => CADENA(str.substring(1, str.length).toUpperCase) } }
  def palabra: Parser[PALABRA] = positioned { "[a-zA-Z][a-zA-Z0-9]*".r ^^ { str => PALABRA(str) } }
  def nativa: Parser[NATIVA] = positioned { "(?i)random|sin|cos|abs".r ^^ { str => NATIVA(str) } }
  def nativa2: Parser[NATIVA2] = positioned { "(?i)mod".r ^^ { str => NATIVA2(str) } }

  def parabi = positioned { "(" ^^^ PARABI() }
  def parcer = positioned { ")" ^^^ PARCER() }
  def corcheteabi = positioned { "[" ^^^ CORCHETEABI() }
  def corchetecer = positioned { "]" ^^^ CORCHETECER() }
  def menorque = positioned { "<" ^^^ MENORQUE() }
  def mayorque = positioned { ">" ^^^ MAYORQUE() }
  def igual = positioned { "==" ^^^ IGUAL() }
  def menorigual = positioned { "<=" ^^^ MENORIGUAL() }
  def mayorigual = positioned { ">=" ^^^ MAYORIGUAL() }
  def distinto = positioned { "!=" ^^^ DISTINTO() }
  def mas = positioned { "+" ^^^ MAS() }
  def menos = positioned { "-" ^^^ MENOS() }
  def prod = positioned { "*" ^^^ PROD() }
  def div = positioned { "/" ^^^ DIV() }
  def forloop = positioned { "(?i)for".r ^^^ FOR() }
  def repeat = positioned { "(?i)repeat".r ^^^ REPEAT() }
  def ifclause = positioned { "(?i)if".r ^^^ IF() }
  def cs = positioned { "(?i)cs|clearscreen".r ^^^ CS() }
  def pu = positioned { "(?i)pu|penup".r ^^^ PU() }
  def pd = positioned { "(?i)pd|pendown".r ^^^ PD() }
  def ht = positioned { "(?i)ht|hideturtle".r ^^^ HT() }
  def st = positioned { "(?i)st|showturtle".r ^^^ ST() }
  def stop = positioned { "(?i)stop".r ^^^ STOP() }
  def make = positioned { "(?i)make".r ^^^ MAKE() }
  def fd = positioned { "(?i)fd|forward".r ^^^ FD() }
  def bk = positioned { "(?i)bk|backward".r ^^^ BK() }
  def rt = positioned { "(?i)rt|right".r ^^^ RT() }
  def lt = positioned { "(?i)lt|left".r ^^^ LT() }
  def arcr = positioned { "(?i)arcr".r ^^^ ARCR() }
  def arcl = positioned { "(?i)arcl".r ^^^ ARCL() }
  def home = positioned { "(?i)home".r ^^^ HOME() }
  def setxy = positioned { "(?i)setxy".r ^^^ SETXY() }
  def to = positioned { "(?i)to".r ^^^ TO() }
  def end = positioned { "(?i)end".r ^^^ END() }

  def tokens: Parser[List[LogoToken]] = {
    phrase(rep(palabra ||| variable ||| numero ||| cadena ||| nativa ||| nativa2 ||| forloop ||| repeat ||| ifclause ||| cs ||| pu ||| pd ||| ht ||| st ||| stop ||| make
      ||| fd ||| bk ||| rt ||| lt ||| arcr ||| arcl ||| home ||| setxy ||| parabi ||| parcer ||| corcheteabi
      ||| corchetecer ||| menorque ||| mayorque ||| igual ||| menorigual ||| mayorigual ||| distinto
      ||| mas ||| menos ||| prod ||| div ||| to ||| end))
  }

  def apply(code: String): Either[LogoLexerError, List[LogoToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => scala.util.Left(LogoLexerError(Location(next.pos.line, next.pos.column),msg))
      case Success(result, _) => scala.util.Right(result)
    }
  }
}