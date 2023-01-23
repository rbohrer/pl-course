package edu.wpi.rbohrer.plcourse
package scalalectures

object Lecture4Main {
  def main(args: Array[String]): Unit = {
    expressionExamples()
    statementExamples()
    definitionExamples()
    typeExamples()
    regexExamples()
  }

  def expressionExamples() : Unit = {
    // values
    12
    12.34
    "hi"
    // expressions
    12
    12.34
    "hi"
    1 + 1
    "hi" + " " + "there"
  }

  def statementExamples() : Unit = {
    // statements (are just expressions in Scala)
    println("Some text")
  }

  def definitionExamples() : Unit = {
    val x = 1 + 2
    var y = x * 7
    val z = x + y
    y = y - z
  }

  def typeExamples() : Unit = {
    val str1 = "Hello"
    val str2 : String = "Hello"
    var x : Int = 5
    var y = x
    x = 7

    var z : Any = 5
    z = "My string"

    /* Some types:
       Any
       Int
       Double
       String
       Boolean */
  }

  def regexExamples() : Unit = {
    val date = raw"(\d{4})-(\d{2})-(\d{2})".r
    val msg =
      "2004-01-20" match {
        case date(year, month, day) => s"$year was a good year for PLs."
      }
    val msg2 =
      "2004-01-20" match {
        case date(_*) => "It's a date!"
      }
    println(msg + " " + msg2)
  }
}
