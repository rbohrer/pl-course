package edu.wpi.rbohrer.plcourse
package parselectures

object PolynomialLetMain {

  private def parseStr(src: String): Expression = {
    parse(src, new FastparsePolynomialLet().expr(_)) match {
      case Parsed.Success(value, index) => value
      case Parsed.Failure(_label, index, _extra) =>
        throw new Exception("Parse error at index " + index)
    }
  }

  private def batchMode(fileName: String): Unit = {
    val src = Source.fromFile(fileName)
    val prog = src.mkString
    src.close()
    val e = parseStr(prog)
    val result = PolynomialLetInterpreter(e)
    println("Program result: ")
    println(result)
  }

  private def interactiveLoop(): Unit = {
    while (true) {
      print("> ")
      val prog = readLine()
      val e = parseStr(prog)
      val result = PolynomialLetInterpreter(e)
      println(result)
    }
  }

  private def interactiveMode(): Unit = {
    println("starting interactive mode, type code!")
    interactiveLoop()
  }

  def main(args: Array[String]): Unit =
    if (args.length > 0) {
      batchMode(args(0))
    } else {
      interactiveMode()
    }
}