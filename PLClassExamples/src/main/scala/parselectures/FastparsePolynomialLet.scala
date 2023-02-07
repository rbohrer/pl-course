package edu.wpi.rbohrer.plcourse
package parselectures

import fastparse.MultiLineWhitespace._
import fastparse._

sealed trait Expression

sealed trait Value extends Expression
final case class ApplyOp(op : Operator, left: Expression, right: Expression) extends Expression
final case class Number(value: Int) extends Value {
  override def toString: String = {
    value.toString
  }
}
final case class Variable(name : String) extends Expression
// let x = e1 in e2
final case class Let(name : String, definition : Expression, body : Expression) extends Expression


sealed trait Operator
case object Plus extends Operator
case object Minus extends Operator
case object Times extends Operator
case object Divide extends Operator


//(1+2)*5
/*
     *
    / \
    +  5
   / \
   1 2
   *
   * ApplyOp(Times, ApplyOp(Plus, Number(1), Number(2)), Number(5))
 */
class FastparsePolynomialLet {

  def number[_: P]: P[Expression] =
    {
      import fastparse.NoWhitespace._
      P( CharIn("0-9").rep(1).!.map(_.toInt).map(Number) )
    }
  def parens[_: P]: P[Expression] = P( "(" ~/ addSub ~ ")" )

  def ident[_: P]: P[String] = {
    import fastparse.NoWhitespace._
    P (CharIn("a-z").rep(1).!)
  }
  def varExpr[_: P]: P[Expression] = P(ident.map(Variable))
  def ws[_: P]: P[Unit] = P(" ".rep(0)) // whitespace
  def factor[_: P]: P[Expression] = P( number | letExpr | varExpr | parens )


  def letExpr[_ : P]: P[Expression] =
    P(("let" ~ ws ~
      ident ~ ws ~
      "=" ~ ws ~
      addSub ~ ws ~
      "in" ~ ws ~ addSub).map(letHelper))

  def divMul[_: P]: P[Expression] = P( factor ~ ws ~(CharIn("*/").! ~/ ws ~ factor).rep ).map(divMulHelper)
  def addSub[_: P]: P[Expression] = P( divMul ~ ws ~ (CharIn("+\\-").! ~/ ws ~ divMul).rep ).map(addSubHelper)
  def expr[_: P]: P[Expression]   = P( addSub ~ ws ~ End )

  def letHelper(x : (String, Expression, Expression)): Expression = {
    val (y, e1,e2) = x
    Let(y,e1,e2)
  }
  def addSubHelper(x : (Expression, Seq[(String, Expression)])): Expression = {
    val (b, xs) = x
    // Anonymous function for combining elements
    xs.foldLeft(b)((acc,next) =>
      next match {
        case  ("+", e) => ApplyOp(Plus, acc, e)
        case  ("-", e) => ApplyOp(Minus, acc, e)
      })
  }
  def divMulHelper(x : (Expression, Seq[(String, Expression)])): Expression = {
    val (b, xs) = x
    // Anonymous function for combining elements
    xs.foldLeft(b)((acc,next) =>
      next match {
        case ("/", e) => ApplyOp(Divide, acc, e)
        case ("*", e) => ApplyOp(Times, acc, e)
      })
  }
}

