package evaluator

import parser.Procedimiento

case class SymbolTable(position: (Double, Double),
                       orientation: Double,
                       isPenUp: Boolean,
                       proc: Map[String, Procedimiento],
                       vars: Map[String, Int])

object SymbolTable {

  val emptyProcs = Map.empty[String, Procedimiento]
  val emptyVars = Map.empty[String, Int]
  val empty = SymbolTable((0,0), 90, true, emptyProcs, emptyVars)

  implicit class SymbolTableOps(a: SymbolTable) {
    def withProcs(procs: Map[String, Procedimiento]) = a.copy(proc = a.proc ++ procs)
    def +(variable: (String, Int)) = a.copy(vars = a.vars + variable)
    def penDown = a.copy(isPenUp = false)
    def penUp = a.copy(isPenUp = true)
    def setPosition(x: Double, y: Double) = a.copy(position = (x, y))
    def setOrientation(angulo: Double) = a.copy(orientation = angulo%360)
    def pprint: String = {
      s"position: ${a.position}\n" +
      s"orientation: ${a.orientation}\n" +
      s"isPenUp: ${a.isPenUp}\n" +
      "vars: " + a.vars.map{
        case (name, value) => s"$name=$value"
      }.mkString(" ")
    }
  }
}

