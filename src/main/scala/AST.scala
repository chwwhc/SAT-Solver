object AST {
    sealed trait Expr 
    
    case class Var(name: String) extends Expr 
    case class And(e1: Expr, e2: Expr) extends Expr 
    case class Or(e1: Expr, e2: Expr) extends Expr 
    case class Not(e1: Expr) extends Expr 
    case class Const(value: Boolean) extends Expr 
}
