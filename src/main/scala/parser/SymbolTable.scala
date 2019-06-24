package parser

import parser.Value.Value

case class SymbolTable(position: (Int, Int),
                       isPenUp: Boolean,
                       proc: Map[String, Procedimiento],
                       vars: Map[String, Value])

object SymbolTable {

  val emptyProcs = Map.empty[String, Procedimiento]
  val emptyVars = Map.empty[String, Value]
  val empty = SymbolTable((0,0), true, emptyProcs, emptyVars)

  implicit class SymbolTableOps(a: SymbolTable) {
    def withProcs(procs: Map[String, Procedimiento]) = a.copy(proc = a.proc ++ procs)
    def +(variable: (String, Value)) = a.copy(vars = a.vars + variable)
    def penDown = a.copy(isPenUp = false)
    def penUp = a.copy(isPenUp = true)
  }
}

