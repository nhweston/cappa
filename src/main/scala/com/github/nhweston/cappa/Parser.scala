package com.github.nhweston.cappa

import com.github.nhweston.cappa.Term.{Abs, App, Var}
import com.github.nhweston.cappa.Parser.abstractions

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers, RegexParsers}

class Parser extends RegexParsers with PackratParsers with ImplicitConversions {

  def apply(program: String): Either[String, Term] =
    parseAll(term, program) match {
      case Success(result, _) => Right(result)
      case e: NoSuccess => Left(e.msg)
    }

  lazy val term: PackratParser[Term] =
    let | abs | app | `var` | parens

  lazy val parens: PackratParser[Term] =
    "(" ~> term <~ ")"

  lazy val abs: PackratParser[Abs] =
    (name ~ rep(name) <~ "=>") ~ term ^^ abstractions

  lazy val app: PackratParser[App] =
    (app | parens | `var`) ~ (parens | `var`) ^^ App

  lazy val let: PackratParser[App] =
    (name ~ rep(name) <~ "=") ~ (term <~ ";") ~ term ^^ {
      case name ~ Nil ~ value ~ body =>
        App(Abs(name, body), value)
      case name ~ (hd :: tl) ~ value ~ body =>
        App(Abs(name, body), abstractions(hd, tl, value))
    }

  lazy val `var`: PackratParser[Var] =
    name ^^ Var

  lazy val name: PackratParser[String] =
    regex("""[a-zA-Z0-9_]+""".r)

}

object Parser {

  val Comment = "(?m)#.*$".r

  def apply(text: String): Either[String, Term] = {
    (new Parser)(Comment.replaceAllIn(text, ""))
  }

  def abstractions(head: String, tail: List[String], body: Term): Abs = {
    def aux(hd: String, tl: List[String]): Abs =
      Abs(hd, tl match {
        case hd :: tl => aux(hd, tl)
        case Nil => body
      })
    aux(head, tail)
  }

}
