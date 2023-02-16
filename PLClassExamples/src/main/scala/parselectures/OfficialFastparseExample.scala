package edu.wpi.rbohrer.plcourse
package parselectures

import fastparse.NoWhitespace._
import fastparse._
object OfficialFastparseExample {

    // evaluate strings to ints
    def eval(tree: (Int, Seq[(String, Int)])) = {
        val (base, ops) = tree
        ops.foldLeft(base){ case (left, (op, right)) => op match{
            case "+" => left + right case "-" => left - right
            case "*" => left * right case "/" => left / right
        }}
    }

    /*
    * Precedence-climbing approach
    * Lowest(1):  +-
    *       (2):  * /
    * Highest(3): Numbers, Parentheses()
    *
    *  Separate parsing into precedence levels
    *  To parse level i:
    *   Either parse op(e_i,e_i) where op is precendence i
    *   or, parse e_{i+1}
    *
    *   (1) + (2*3) + (4/5)
    * */

    def number[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
    def parens[_: P]: P[Int] = P( "(" ~/ addSub ~ ")" )
    def factor[_: P]: P[Int] = P( parens | number)

    def op[_: P]: P[String] = P( CharIn("+").! | CharIn("\\-").! )
    def divMul[_: P]: P[Int] =
        P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
    def addSub[_: P]: P[Int] =
        P( divMul ~ (CharIn("+\\-").! ~/ divMul).rep ).map(eval)
    def expr[_: P]: P[Int]   = P( addSub ~ End )

    def badExpr[_: P]: P[Unit]   = P(
          (badExpr ~ "*" ~ badExpr)
        | (badExpr ~ "+" ~ badExpr)
    )

    def doTests():Unit = {
        val _ = parse("1+1", badExpr(_))
        val Parsed.Success(2, _) = parse("1+1", expr(_))
        val Parsed.Success(15, _) = parse("(1+1*2)+3*4", expr(_))
        val Parsed.Success(21, _) = parse("((1+1*2)+(3*4*5))/3", expr(_))
        val Parsed.Failure(expected, failIndex, extra) =
             parse("1+1*", expr(_))
        val longAggMsg = extra.trace().longAggregateMsg
        assert(
          failIndex == 4,
          longAggMsg ==
            """Expected expr:1:1 / addSub:1:1 / divMul:1:3 / factor:1:5 /
              | (number | parens):1:5, found """"".stripMargin
        )
    }

    def main(args: Array[String]):Unit = {
        doTests()
    }
}
