import AST.*

object Solver {
  private def getFreeVar(e: Expr): Option[String] = {
    e match
      case Var(name) => Some(name)
      case And(e1, e2) =>
        getFreeVar(e1) match
          case None        => getFreeVar(e2)
          case Some(value) => Some(value)
      case Or(e1, e2) =>
        getFreeVar(e1) match
          case None        => getFreeVar(e2)
          case Some(value) => Some(value)
      case Not(e1)      => getFreeVar(e1)
      case Const(value) => None
  }

  private def guessVar(var_name: String)(candidate: Boolean)(e: Expr): Expr = {
    val guess: (Expr => Expr) = guessVar(var_name)(candidate)
    e match
      case Var(name)    => if (name == var_name) Const(candidate) else Var(name)
      case And(e1, e2)  => And(guess(e1), guess(e2))
      case Or(e1, e2)   => Or(guess(e1), guess(e2))
      case Not(e1)      => Not(guess(e1))
      case Const(value) => Const(value)
  }

  private def extractConst(e: Expr): Boolean = {
    e match
      case Const(value) => value
      case _            => throw new IllegalArgumentException
  }

  private def simplify(e: Expr): Expr = {
    e match
      case Var(_) | Const(_) => e
      case And(e1, e2) => {
        val filtered: List[Expr] =
          (simplify(e1) :: simplify(e2) :: Nil).filter(_ != Const(true))

        if (filtered.contains(Const(false))) Const(false)
        else
          (filtered match
            case e1 :: e2 :: tail => And(e1, e2)
            case e1 :: tail       => e1
            case Nil              => Const(true)
          )
      }
      case Or(e1, e2) => {
        val filtered =
          (simplify(e1) :: simplify(e2) :: Nil).filter(_ != Const(false))

        if (filtered.contains(Const(true))) Const(true)
        else
          (filtered match
            case e1 :: e2 :: tail => Or(e1, e2)
            case e1 :: tail       => e1
            case Nil              => Const(false)
          )
      }
      case Not(e1) =>
        e1 match
          case Const(value) => Const(!value)
          case other        => Not(other)
  }

  def isSatisfiable(e: Expr): Boolean = {
    getFreeVar(e) match
      case None => extractConst(e)
      case Some(name) =>
        isSatisfiable(simplify(guessVar(name)(true)(e))) || isSatisfiable(
          simplify(guessVar(name)(false)(e))
        )
  }
}
