package edu.wpi.rbohrer.plcourse
package parselectures

import dummy.dummy

class InterpException (orig : Exception) extends Exception
object PolynomialLetInterpreter {
  type Environment = Map[String,Value]

  private def applyOp(operator: Operator, l : Value, r : Value): Value = {
    (operator, l, r) match {
      case (Plus, nl : Number, nr : Number) => Number(nl.value + nr.value)
      case (Minus, nl : Number, nr : Number) => Number(nl.value - nr.value)
      case (Times, nl : Number, nr : Number) => Number(nl.value * nr.value)
      case (Divide, nl : Number, nr : Number) =>
      try {
        Number(nl.value / nr.value)
      } catch { case e : ArithmeticException => throw new InterpException(e)}
    }
  }

  def apply(env: Environment, e: Expression): Value = {
    e match {
      case value: Value => value
      case Variable(name) =>
      try {
        env(name)
      } catch { case e : NoSuchElementException => throw new InterpException(e)}
      case ApplyOp(op, left, right) => applyOp(op,apply(env,left),apply(env,right))
      case Let(name, definition, body) =>
        val v = apply(env, definition)
        val env2 = env + (name -> v)
        apply(env2, body)
    }
  }

  def apply(e: Expression): Value = { apply(Map(), e) }

}
