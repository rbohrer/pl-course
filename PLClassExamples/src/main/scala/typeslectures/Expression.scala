/* Defines the AST for the language used in these lectures */
package edu.wpi.rbohrer.plcourse
package typeslectures


sealed trait Expression
sealed trait Value extends Expression
sealed trait BoolValue extends Value

final case class Number(value: Int) extends Value {
  override def toString: String = {
    value.toString
  }
}
case object True extends BoolValue
case object False extends BoolValue
case object UnitTuple extends Value

final case class ApplyOp(op : Operator, left: Expression, right: Expression) extends Expression
final case class Variable(name : String) extends Expression
final case class Let(definition : Definition, body : Expression) extends Expression
final case class Seq(left: Expression, right: Expression) extends Expression
final case class ApplyFun(f: String, arg: Expression) extends Expression

sealed trait Operator
case object Plus extends Operator
case object Minus extends Operator
case object Times extends Operator
case object Divide extends Operator

sealed trait Definition
final case class ValDef(name: String, rhs: Expression) extends Definition
final case class FunDef(name: String, argName: String, argType: Type, rhs: Expression) extends Definition

sealed trait Type
case object Num extends Type
case object Bool extends Type
case object Unit extends Type
case class FunType(arg: Type, result: Type) extends Type



