import AST.*
import Solver.*

@main def hello: Unit =
  val e1 = ((
    isSatisfiable(
      And(
        And(
          Var("x"),
          And(
            And(
              Var("x"),
              And(And(And(Var("x"), Var("x")), Var("x")), Var("x"))
            ),
            Var("x")
          )
        ),
        Var("x")
      )
    )
  ))
  println(e1)
