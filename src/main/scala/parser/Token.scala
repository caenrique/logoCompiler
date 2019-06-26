package parser

import scala.util.parsing.input.Positional

sealed trait LogoToken extends Positional

case class VARIABLE(str: String) extends LogoToken
case class NUMERO(value: Int) extends LogoToken
case class CADENA(str: String) extends LogoToken
case class PALABRA(str: String) extends LogoToken
case class NATIVA(str: String) extends LogoToken
case class NATIVA2(str: String) extends LogoToken
case class PARABI() extends LogoToken
case class PARCER() extends LogoToken
case class CORCHETEABI() extends LogoToken
case class CORCHETECER() extends LogoToken
case class MENORQUE() extends LogoToken
case class MAYORQUE() extends LogoToken
case class IGUAL() extends LogoToken
case class MENORIGUAL() extends LogoToken
case class MAYORIGUAL() extends LogoToken
case class DISTINTO() extends LogoToken
case class MAS() extends LogoToken
case class MENOS() extends LogoToken
case class PROD() extends LogoToken
case class DIV() extends LogoToken
case class FOR() extends LogoToken
case class REPEAT() extends LogoToken
case class IF() extends LogoToken
case class CS() extends LogoToken
case class PU() extends LogoToken
case class PD() extends LogoToken
case class HT() extends LogoToken
case class ST() extends LogoToken
case class STOP() extends LogoToken
case class MAKE() extends LogoToken
case class FD() extends LogoToken
case class BK() extends LogoToken
case class RT() extends LogoToken
case class LT() extends LogoToken
case class ARCR() extends LogoToken
case class ARCL() extends LogoToken
case class HOME() extends LogoToken
case class SETXY() extends LogoToken
case class TO() extends LogoToken
case class END() extends LogoToken
