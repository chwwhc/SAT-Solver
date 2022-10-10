import AST.*

@main def hello: Unit = 
  val var1 = Const(false)
  val var2 = Const(true)
  println(var1 == var2)