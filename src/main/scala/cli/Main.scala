package cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import compiler.LogoCompiler

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val filename = args(0) // Gestionar esto bien
    val logoCode = Source.fromFile(filename).mkString
    LogoCompiler(logoCode) match {
      case Left(err) => println(err)
      case Right(code) => Files.write(Paths.get("out.html"), code.getBytes(StandardCharsets.UTF_8))
    }
  }

}
