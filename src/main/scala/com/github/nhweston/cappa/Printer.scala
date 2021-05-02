package com.github.nhweston.cappa

import com.github.nhweston.cappa.Term.{Abs, App, Var}

object Printer extends (Term => String) {

  def apply(term: Term): String = {
    def aux(term: Term, parenthesise: Boolean): String = {
      val result =
        term match {
          case Var(name) =>
            name
          case App(func, arg) =>
            aux(func, func.isInstanceOf[Abs]) + " " + aux(arg, !arg.isInstanceOf[Var])
          case Abs(name, body) =>
            name + " => " + Printer(body)
        }
      if (parenthesise) s"($result)" else result
    }
    aux(term, false)
  }

}
