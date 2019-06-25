package parser

import errors.LogoCompilationError.{Location, LogoParserError}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object LogoParser extends Parsers {
  override type Elem = LogoToken

  class LogoTokenReader(tokens: Seq[LogoToken]) extends Reader[LogoToken] {
    override def first: LogoToken = tokens.head
    override def rest: Reader[LogoToken] = new LogoTokenReader(tokens.tail)
    override def pos: Position = if (atEnd) NoPosition else first.pos
    override def atEnd: Boolean = tokens.isEmpty

  }

  private def variable: Parser[VARIABLE] = {
    accept("variable", { case v @ VARIABLE(name) => v })
  }

  private def numero: Parser[NUMERO] = {
    accept("numero", { case n @ NUMERO(value) => n })
  }

  private def cadena: Parser[CADENA] = {
    accept("cadena", { case c @ CADENA(str) => c })
  }

  private def palabra: Parser[PALABRA] = {
    accept("palabra", { case p @ PALABRA(str) => p })
  }

  private def nativa: Parser[NATIVA] = {
    accept("nativa", { case n @ NATIVA(str) => n })
  }

  def ast: Parser[LogoAST] = {
    phrase(programa)
  }

  def programa: Parser[Programa] = {
    procedimiento.* ~ instruccion.* ^^ { case procedimientos ~ instrucciones => Programa(procedimientos, instrucciones) }
  }

  def procedimiento: Parser[Procedimiento] = {
    TO() ~ palabra ~ listaParams ~ bloque ^^ { case _ ~ PALABRA(str) ~ params ~ bloque => Procedimiento(str, params, bloque) }
  }

  def listaParams: Parser[ListaParams] = {
    variable.* ^^ (variables => ListaParams(variables.map { case VARIABLE(str) => Variable(str) }))
  }

  def instruccion: Parser[Instruccion] = {
    mover | comando | bucles | llamadaProcedimiento
  }

  def mover: Parser[Mover] = {
    val home = HOME() ^^^ Home
    val forward = FD() ~ expresion ^^ { case _ ~ expr => Forward(expr) }
    val backward = BK() ~ expresion ^^ { case _ ~ expr => Backward(expr) }
    val right = RT() ~ expresion ^^ { case _ ~ expr => Right(expr) }
    val left = LT() ~ expresion ^^ { case _ ~ expr => Left(expr) }
    val setxy = SETXY() ~ expresion ~ expresion ^^ { case _ ~ expr1 ~ expt2 => SetXY(expr1, expt2) }
    val rightArc = ARCR() ~ expresion ~ expresion ^^ { case _ ~ rad ~ ang => RightArc(rad, ang) }
    val leftArc = ARCL() ~ expresion ~ expresion ^^ { case _ ~ rad ~ ang => LeftArc(rad, ang) }
    forward | backward | right | left | rightArc | leftArc | setxy | home
  }

  def comando: Parser[Comando] = {
    val make = MAKE() ~ cadena ~ expresion ^^ { case _ ~ CADENA(str) ~ expr => Make(str, expr) }
    CS() ^^^ ClearScreen | PU() ^^^ PenUp | PD() ^^^ PenDown | HT() ^^^ HideTurtle | ST() ^^^ ShowTurtle | STOP() ^^^ Stop | make
  }

  def bucles: Parser[Bucles] = {
    val forloop = FOR() ~ CORCHETEABI() ~ palabra ~ expresion ~ expresion ~ expresion ~ CORCHETECER() ~ bloque ^^ {
      case _ ~ _ ~ PALABRA(str) ~ inicio ~ fin ~ inc ~ _ ~ bloq => For(str, inicio, fin, inc, bloq)
    }

    val repeat = REPEAT() ~ expresion ~ bloque ^^ { case _ ~ expr1 ~ bloq => Repeat(expr1, bloq) }

    val ifclause = IF() ~ condicion ~ bloque ^^ { case _ ~ cond ~ bloq => If(cond, bloq) }

    forloop | repeat | ifclause
  }

  def llamadaProcedimiento: Parser[LlamadaProcedimiento] = {
    palabra ~ expresion.* ^^ { case PALABRA(str) ~ expresiones => LlamadaProcedimiento(str, expresiones) }
  }

  def condicion: Parser[Condicion] = {
    val menorque = expresion ~ MENORQUE() ~ expresion ^^ { case l ~ _ ~ r => MenorQue(l, r) }
    val mayorque = expresion ~ MAYORQUE() ~ expresion ^^ { case l ~ _ ~ r => MayorQue(l, r) }
    val igual = expresion ~ IGUAL() ~ expresion ^^ { case l ~ _ ~ r => Igual(l, r) }
    val menorigual = expresion ~ MENORIGUAL() ~ expresion ^^ { case l ~ _ ~ r => MenorIgual(l, r) }
    val mayorigual = expresion ~ MAYORIGUAL() ~ expresion ^^ { case l ~ _ ~ r => MayorIgual(l, r) }
    val distinto = expresion ~ DISTINTO() ~ expresion ^^ { case l ~ _ ~ r => Distinto(l, r) }

    menorque | mayorque | igual | menorigual | mayorigual | distinto
  }

  def bloque: Parser[Bloque] = {
    CORCHETEABI() ~ instruccion.* ~ CORCHETECER() ^^ { case _ ~ instrucciones ~ _ => Bloque(instrucciones) }
  }

  def expresion: Parser[Expresion] = {
    termino ~ ep.? ^^ { case t ~ ep => Expresion(t, ep) }
  }

  def ep: Parser[Ep] = {
    val mas = MAS() ~ termino ~ ep.? ^^ { case _ ~ t ~ ep => Mas(t, ep) }
    val menos = MENOS() ~ termino ~ ep.? ^^ { case _ ~ t ~ ep => Menos(t, ep) }
    mas | menos
  }

  def termino: Parser[Termino] = {
    factor ~ tp.? ^^ { case f ~ tp => Termino(f, tp) }
  }

  def factor: Parser[Factor] = {
    val num = numero ^^ { case NUMERO(n) => Numero(n) }
    val vari = variable ^^ { case VARIABLE(str) => Variable(str) }
    val cad = cadena ^^ { case CADENA(str) => Cadena(str) }
    val parentesisExpr = PARABI() ~ expresion ~ PARCER() ^^ { case _ ~ expr ~ _ => ParentesisExpr(expr) }
    val nativaExpr = nativa ~ expresion ^^ { case NATIVA(str) ~ expr => NativaExpr(str, expr) }
    nativaExpr | num | vari | cad | parentesisExpr
  }

  def tp: Parser[Tp] = {
    val prod = PROD() ~ factor ~ tp.? ^^ { case _ ~ f ~ tp => Por(f, tp) }
    val div = DIV() ~ factor ~ tp.? ^^ { case _ ~ f ~ tp => Dividido(f, tp) }
    prod | div
  }

  def apply(tokens: Seq[LogoToken]): Either[LogoParserError, LogoAST] = {
    val reader = new LogoTokenReader(tokens)
    ast(reader) match {
      case NoSuccess(msg, next) => scala.util.Left(LogoParserError(Location(next.pos.line, next.pos.column),msg))
      case Success(result, _) => scala.util.Right(result)
    }
  }
}
