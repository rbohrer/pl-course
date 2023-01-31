package edu.wpi.rbohrer.plcourse
package scalalectures

import scala.io.StdIn.readLine

object Lecture5Main {
  def regexExamples(): Unit = {

    val date = raw"(\d{4})-(\d{2})-(\d{2})".r
    val msg =
      "2004-01-20" match {
        case date(year, month, day) => s"$year was a good year for PLs."
      }
    val msg2 =
      "2004-01-20" match {
        case date(_*) => "It's a date!"
      }
    val logMsg =
      List("info: CoolProgram initialized without error",
        "debug: CoolProgram version: v13",
        "debug: UncoolLibrary v420.69 loaded without error",
        "error: UncoolLibrary version cannot be newer than CoolProgram version")
      val re = raw"error: (.*)".r
      // succeed on error message
      logMsg(3) match {
        case re(theMsg) => println("THE ERROR MESSAGE WAS " + theMsg)
      }
      // error
      //logMsg(0) match {
      //  case re(theMsg) => println("THE ERROR MESSAGE WAS " + theMsg)
      //}

      // error handled
      logMsg(0) match {
        case re(theMsg) => println("THE ERROR MESSAGE WAS " + theMsg)
        case _ => println("not a  error")
      }

    println(msg + " " + msg2)
  }

  def main(args: Array[String]): Unit = {
   regexExamples()
  }
}
