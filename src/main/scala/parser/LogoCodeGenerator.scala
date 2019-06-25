package parser

import parser.EvalData.EvalResult
import parser.LogoCompilationError.LogoEvaluationError
import parser.Monoid._

object LogoCodeGenerator {

  import SymbolTable._

  import scala.util.{Left, Right}
  // registrar la posicion de la tortuga y el estado del pen
  // Asumo que no existen ámbitos. Todas las variables son globales y se pueden leer si están definidas previamente.
  // Una vez que se definen, permanecen definidas hasta el final de la ejecución

  def apply(ast: LogoAST): EvalResult = ast match {
    case Programa(proc, instr) =>
      val procs = registraProcedimientos(proc)
      val startEvalData = EvalData(Right(""), SymbolTable.empty.withProcs(procs))

      instruccionesEval(instr, startEvalData).result
  }

  def registraProcedimientos(value: List[Procedimiento]): Map[String, Procedimiento] = ???

  def instruccionesEval(instrucciones: List[Instruccion], startEvalData: EvalData): EvalData = {
    instrucciones.foldLeft(startEvalData) {
      case (evalData, nextInstr) => evalData + instruccionEval(nextInstr, evalData.symbols)
    }
  }

  def instruccionEval(instruccion: Instruccion, simbolos: SymbolTable): EvalData = instruccion match {
    case m: Mover => moverEval(m, simbolos)
    case c: Comando => comandoEval(c, simbolos)
    case b: Bucles => buclesEval(b, simbolos)
    case p: LlamadaProcedimiento => ???
  }

  def moverEval(mover: Mover, simbolos: SymbolTable): EvalData = mover match {
    case Forward(expr) => ExpresionEvaluator.evalNumber(expr, simbolos) { distancia: Int =>
      val dx = distancia * Math.cos(simbolos.orientation.toRadians)
      val dy = distancia * Math.sin(simbolos.orientation.toRadians)
      val newX = (simbolos.position._1 + dx).toInt
      val newY = (simbolos.position._2 + dy).toInt

      val code = if(simbolos.isPenUp) s"moveTo($newX, $newY)" else s"lineTo($newX, $newY)"
      EvalData(Right(code), simbolos.setPosition(newX, newY))
    }
    case Backward(expr) => ExpresionEvaluator.evalNumber(expr, simbolos) { distancia: Int =>
      val dx = distancia * Math.cos(simbolos.orientation.toRadians)
      val dy = distancia * Math.sin(simbolos.orientation.toRadians)
      val newX = (simbolos.position._1 - dx).toInt
      val newY = (simbolos.position._2 - dy).toInt

      val code = if(simbolos.isPenUp) s"moveTo($newX, $newY)" else s"lineTo($newX, $newY)"
      EvalData(Right(code), simbolos.setPosition(newX, newY))
    }
    case parser.Right(expr) => ExpresionEvaluator.evalNumber(expr, simbolos) { angulo: Int =>
      val newSimbolos = simbolos.setOrientation(simbolos.orientation - angulo)
      EvalData.withSymbols(newSimbolos)
    }
    case parser.Left(expr) => ExpresionEvaluator.evalNumber(expr, simbolos) { angulo: Int =>
      val newSimbolos = simbolos.setOrientation(simbolos.orientation + angulo)
      EvalData.withSymbols(newSimbolos)
    }
    case LeftArc(radio, angulo) => ExpresionEvaluator.eval2Numbers(radio, angulo, simbolos) { (r: Int, a: Int) =>
      val anguloActual = simbolos.orientation
      val anguloHaciaOrigen = (anguloActual + 90) % 360
      val dx = (r * Math.cos(anguloHaciaOrigen.toRadians)).toInt
      val dy = (r * Math.sin(anguloHaciaOrigen.toRadians)).toInt
      val xArco = simbolos.position._1 + dx
      val yArco = simbolos.position._2 + dy
      val anguloInicial = (anguloHaciaOrigen + 180) % 360
      val anguloFinal = (anguloInicial + a) % 360
      val nuevaOrientacion = (anguloActual + a) % 360
      val nuevoX = xArco + (r * Math.cos((anguloActual - 90 + a).toRadians)).toInt
      val nuevoY = yArco + (r * Math.sin((anguloActual - 90 + a).toRadians)).toInt

      val code = if(simbolos.isPenUp) s"moveTo($nuevoX, $nuevoY)" else s"arc($xArco, $yArco, $r, $anguloInicial, $anguloFinal)"
      val newSymbols = simbolos.setPosition(nuevoX, nuevoY).setOrientation(nuevaOrientacion)
      EvalData(scala.util.Right(code), newSymbols)
    }
    case RightArc(radio, angulo) => ExpresionEvaluator.eval2Numbers(radio, angulo, simbolos) { (r: Int, a: Int) =>
      val anguloActual = simbolos.orientation
      val anguloHaciaOrigen = (anguloActual - 90) % 360
      val dx = (r * Math.cos(anguloHaciaOrigen.toRadians)).toInt
      val dy = (r * Math.sin(anguloHaciaOrigen.toRadians)).toInt
      val xArco = simbolos.position._1 + dx
      val yArco = simbolos.position._2 + dy
      val anguloInicial = (anguloHaciaOrigen + 180) % 360
      val anguloFinal = (anguloInicial - a) % 360
      val nuevaOrientacion = (anguloActual - a) % 360
      val nuevoX = xArco + (r * Math.cos((anguloActual + 90 - a).toRadians)).toInt
      val nuevoY = yArco + (r * Math.sin((anguloActual + 90 - a).toRadians)).toInt

      val code = if(simbolos.isPenUp) s"moveTo($nuevoX, $nuevoY)" else s"arc($xArco, $yArco, $r, $anguloInicial, $anguloFinal)"
      val newSymbols = simbolos.setPosition(nuevoX, nuevoY).setOrientation(nuevaOrientacion)
      EvalData(scala.util.Right(code), newSymbols)
    }
    case Home => EvalData.withSymbols(simbolos.setPosition(0, 0))
    case SetXY(x, y) => ExpresionEvaluator.eval2Numbers(x, y, simbolos, "No se pueden usar cadenas como posicion") {
      (xv, yv) => EvalData.withSymbols(simbolos.setPosition(xv, yv))
    }
  }

  def comandoEval(comando: Comando, simbolos: SymbolTable): EvalData = comando match {
    case ClearScreen => EvalData(Right("clearScreen"), simbolos)
    case PenDown => EvalData.withSymbols(simbolos.penDown)
    case PenUp => EvalData.withSymbols(simbolos.penUp)
    case HideTurtle => ???
    case ShowTurtle => ???
    case Stop => ???
    case Make(varName, expr) => ExpresionEvaluator(expr, simbolos) match {
      case Left(err) => EvalData(Left(err), simbolos)
      case Right(value) => EvalData.withSymbols(simbolos + (varName, value))
    }
  }

  def buclesEval(bucles: Bucles, table: SymbolTable): EvalData = {
    def forEval(forLoop: For, simbolos: SymbolTable): EvalData = ???

    def repeatEval(repeat: Repeat, simbolos: SymbolTable): EvalData = ???

    def ifEval(ifClause: If, simbolos: SymbolTable): EvalData = ???

    ???
  }

  def bloqueEval(bloque: Bloque, simbolos: SymbolTable): EvalData = {
    instruccionesEval(bloque.instrucciones, EvalData(Right(""), simbolos))
  }

}
