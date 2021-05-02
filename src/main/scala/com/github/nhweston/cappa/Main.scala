package com.github.nhweston.cappa

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit =
    args.toSeq match {
      case filename +: Nil =>
        val src = Source.fromFile(filename)
        val text = src.getLines().mkString
        src.close()
        println(Parser(text).map(Printer.compose(Term.normalise)).merge)
    }

}
