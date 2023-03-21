package edu.wpi.rbohrer.plcourse
package parselectures

object Elaborator {
  def apply(e : Expression): Expression = {
    e match {
      case value: Value => ???
      case ApplyOp(op, left, right) => ???
      case Variable(name) => ???

    }
    e match {
      case ApplyOp(op, left, right) =>
        val l = apply(left)
        val r = apply(right)
        (op,l,r) match {
          case (Power, _, rnum : Number) if rnum.value == 1 => l
          case (Power, _, rnum : Number) if rnum.value > 1 => ApplyOp(Times,l,apply(ApplyOp(Power,l,Number(r.value-1))))
          case _ => ApplyOp(op,l,r)
        }
      case Let(name, definition, body) => Let(name,apply(definition), apply(body))
      case LetFun(fn, body) => LetFun(fn,apply(body))
      case ApplyFun(fn, args) => ApplyFun(apply(fn), args.map(apply))
      case x => x
      }

  }
}
