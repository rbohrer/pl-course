package edu.wpi.rbohrer.plcourse
package typeslectures


object TypeChecker {
  type Context = Map[String, Type]
  case class IllTypedException(C:Context, e: Expression) extends Exception

  def apply(e: Expression): Type = apply(Map(),e)

  def apply(C: Context, e: Expression): Type = {
    e match {
      case _: Number => Num
      case _: BoolValue => Bool
      case UnitTuple => Unit
      case ApplyOp(op, left, right) =>
        val tLeft = apply(C,left)
        val tRight = apply(C,right)
        (tLeft, tRight) match {
          case (Num, Num) => Num
          case _ => throw IllTypedException(C,e)
        }
      case Variable(name) => C(name)
      case Let(definition, body) =>
        val (name,t) = defType(C,definition)
        apply(C + (name -> t), body)
      case Seq(left, right) =>
        apply(C, left);
        apply(C, right)
      case ApplyFun(f, arg) =>
        (C(f), apply(C, arg)) match {
          case (FunType(t1,t2), t3:Type) if t1 == t3 => t2
          case _ => throw IllTypedException(C,e)
        }
    }

    def defType(C: Context, d : Definition) : (String, Type) = {
      d match {
        case ValDef(name, rhs) => (name, apply(C,rhs))
        case FunDef(name, argName, argType, rhs) =>
          (name, FunType(argType,apply(C + (argName -> argType), rhs)))
      }
    }
  }

}
