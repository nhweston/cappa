package com.github.nhweston.cappa

import scala.annotation.tailrec

sealed trait Term

object Term {

  case class Abs(name: String, body: Term) extends Term

  case class App(func: Term, arg: Term) extends Term

  case class Var(name: String) extends Term

  object fresh {
    val Name = """([a-zA-Z0-9_]+)(?:\$([a-z0-9]+))?""".r
    var counter = 0
    def apply(name: String): String = {
      val suffix = counter.toString
      counter += 1
      val Name(n, _) = name
      s"$n$$$suffix"
    }
  }

  /** Repeatedly applies outermost reduction until the term is in beta normal form. */
  @tailrec
  def normalise(root: Term): Term =
    reduceOnce(root) match {
      case Some(term) => normalise(term)
      case None => root
    }

  /** Reduces the outermost redex. Returns `None` if the term is in beta normal form. */
  def reduceOnce(root: Term): Option[Term] = {
    println(Printer(root))
    root match {
      case Abs(iden, body) =>
        reduceOnce(body).map(Abs(iden, _))
      case App(Abs(name, body), arg) =>
        Some(reduce(name, body, arg))
      case App(func, arg) =>
        reduceOnce(func).map(App(_, arg)).orElse(reduceOnce(arg).map(App(func, _)))
      case Var(_) =>
        None
    }
  }

  /** Reduces the given redex. */
  def reduce(name: String, body: Term, arg: Term): Term = {
    def aux(body: Term): Term =
      body match {
        case Abs(n, body) => Abs(n, aux(body))
        case App(func, arg) => App(aux(func), aux(arg))
        case Var(n) => if (n == name) arg else Var(n)
      }
    val renamings = freeVars(arg).iterator.map(n => n -> fresh(n)).toMap
    aux(rename(body, renamings))
  }

  /** Returns all free variables in the given term. */
  def freeVars(term: Term, bound: Set[String] = Set.empty): Set[String] =
    term match {
      case Abs(name, body) => freeVars(body, bound + name)
      case App(func, arg) => freeVars(func) ++ freeVars(arg)
      case Var(name) => if (bound(name)) Set.empty else Set(name)
    }

  /** Renames non-free variables. */
  def rename(term: Term, renamings: Map[String, String]): Term = {
    def aux(term: Term, bound: Set[String] = Set.empty): Term =
      term match {
        case Abs(name, body) =>
          renamings.get(name) match {
            case Some(n) => Abs(n, aux(body, bound + name))
            case None => Abs(name, aux(body, bound))
          }
        case App(func, arg) =>
          App(aux(func, bound), aux(arg, bound))
        case Var(name) =>
          if (bound(name)) Var(renamings(name))
          else Var(name)
      }
    aux(term)
  }

}
