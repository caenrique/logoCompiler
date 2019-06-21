package parser

sealed trait LogoAST


/*
BloquePrograma --> (Procedimiento)* (Instruccion)*

Procedimiento --> <TO> <PALABRA> ListaParams BloquePrograma <END>

ListaParams --> (<VARIABLE>)*

Instruccion --> Mover | Comando | Bucles | LlamadaProcedimiento
*/


case class Programa(procedimientos: List[Procedimiento], instrucciones: List[Instruccion]) extends LogoAST

case class Procedimiento(nombre: String, parametros: ListaParams, bloque: Bloque)
case class ListaParams(parametros: List[Variable])

sealed trait Instruccion

sealed trait Mover extends Instruccion
case class Forward(expr: Expresion) extends Mover
case class Backward(expr: Expresion) extends Mover
case class Right(expr: Expresion) extends Mover
case class Left(expr: Expresion) extends Mover
case class LeftArc(radio: Expresion, angulo: Expresion) extends Mover
case class RightArc(radio: Expresion, angulo: Expresion) extends Mover
case object Home extends Mover
case class SetXY(x: Expresion, y: Expresion) extends Mover

sealed trait Comando extends Instruccion
case object ClearScreen extends Comando
case object PenUp extends Comando
case object PenDown extends Comando
case object HideTurtle extends Comando
case object ShowTurtle extends Comando
case object Stop extends Comando
case class Make(variable: String, expr: Expresion) extends Comando

sealed trait Bucles extends Instruccion
case class For(i: String, inicio: Expresion, fin: Expresion, inc: Expresion, exec: Bloque) extends Bucles
case class Repeat(expr: Expresion, exec: Bloque) extends Bucles
case class If(condicion: Condicion, exec: Bloque) extends Bucles

case class LlamadaProcedimiento(nombre: String, parametros: List[Expresion]) extends Instruccion

case class Bloque(instrucciones: List[Instruccion])

sealed trait Condicion
case class MenorQue(l: Expresion, r: Expresion) extends Condicion
case class MayorQue(l: Expresion, r: Expresion) extends Condicion
case class Igual(l: Expresion, r: Expresion) extends Condicion
case class MenorIgual(l: Expresion, r: Expresion) extends Condicion
case class MayorIgual(l: Expresion, r: Expresion) extends Condicion
case class Distinto(l: Expresion, r: Expresion) extends Condicion

case class Expresion(termino: Termino, ep: Option[Ep])

sealed trait Ep
case class Mas(termino: Termino, ep: Option[Ep]) extends Ep
case class Menos(termino: Termino, ep: Option[Ep]) extends Ep

case class Termino(factor: Factor, tp: Option[Tp])

sealed trait Tp
case class Por(factor: Factor, tp: Option[Tp]) extends Tp
case class Dividido(factor: Factor, tp: Option[Tp]) extends Tp

sealed trait Factor
case class Numero(valor: Int) extends Factor
case class Variable(nombre: String) extends Factor
case class Cadena(valor: String) extends Factor
case class NativaExpr(funcion: String, parametro: Expresion) extends Factor
case class ParentesisExpr(expr: Expresion) extends Factor



