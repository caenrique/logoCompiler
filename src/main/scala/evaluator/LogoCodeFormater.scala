package evaluator

import scala.io.Source

object LogoCodeFormater {

  def apply(instrucciones: List[String]): String = {
    val separator = ";\n"
    val prefix = "ctx"

    val code = instrucciones.map(i => prefix + "." + i).mkString("", separator, separator)

    Source.fromResource("plantilla.html").mkString.replaceAll("\\{CONTEXT_NAME\\}", prefix).replace("{CODE}", code)
  }

}
