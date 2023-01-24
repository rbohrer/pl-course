package edu.wpi.rbohrer.plcourse
package scalalectures

import scala.io.StdIn.readLine


sealed trait Recipient {
  def angryString : String = toString + "!!!!!"
} // Gillian.angryString

// Gillian.angryString

// this = Gillian
// toString => Gillian.toString => "Gillian"
case object Gillian extends Recipient
case object Craig extends Recipient
case object Charlie extends Recipient


sealed trait MyList {}
case object MyEmptyList extends MyList
case class MyCons(first: Int, rest: MyList) extends MyList

object Lecture4Main {




  def mySum(list : MyList): Int = {
    list match {
      case MyEmptyList => 0
      case MyCons(first, rest) =>
        first + mySum(rest)
    }
  }

  def greetRecipient(rec : Recipient): String = {
    rec match {
      case Gillian => "hi gillian uwu ~~~~"
      case Craig => "Dear Craig"
      case Charlie => "Hi Charlie"
    }
  }


  def arrayExample(): Unit = {
    val myArr : Array[Int] = Array.tabulate(10)(x => x^2)

    var mlyInt = myArr(4)
    val myArr2 = myArr.appended(4)
  }

  def sadGPT() = {
    var input = greetRecipient(Gillian)
    do {
      input = readLine()
      input match {
        case "how are you doing?" =>
          println("i'm saaaaaaaad")
        case "how are the diodes down your left leg" =>
          println("we don't talk about those")
        case msg =>
          println(s"I'm sorry, I didn't quite understand \"$msg\"")
      }
    } while (input != "dogs are better than cats")
    println("you have committed an unforgivable offense")
  }
  def main(args: Array[String]): Unit = {
    /*expressionExamples()
    statementExamples()
    definitionExamples()
    typeExamples()
    regexExamples()*/

    sadGPT()
    readLine()
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
    var z = x + y
    val _ = 8
    z = y - z
    y = z
  }

  def typeExamples() : Unit = {
    val str1 = "Hello"
    val str2 : String = "Hello"
    var x : Int = 5
    var y = x

    var ahhhhh = ???
    x = 7

    var z : Any = 5
    z = "My string"

    /* Some types:
       Any   example values: 0   2.5  "mystring" ()
       Int   values:    4 -3
       Double values:   123.456 -47.3
       String values:    "hello"  "goodbye"
       Boolean:          true false
       Unit:             ()
        */
    if ( x == y) {
      y = y +3
    } else {
      z = y
    }
    var i = 0
    while (i != 10) {
      println("???")
      i = i + 1
    }

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
