package com.github.nhweston.cappa.test

import com.github.nhweston.cappa.{Parser, Term}
import com.github.nhweston.cappa.Term._
import org.scalatest.EitherValues
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpecLike

class ParserTests extends AnyWordSpecLike with should.Matchers with EitherValues {

  "abstractions" should {

    "create a single abstraction" in {
      val actual = Parser.abstractions("a", Nil, Var("x"))
      val expected = Abs("a", Var("x"))
      actual shouldEqual expected
    }

    "create a nested abstraction" in {
      val actual = Parser.abstractions("a", "b" :: Nil, Var("x"))
      val expected = Abs("a", Abs("b", Var("x")))
      actual shouldEqual expected
    }

    "create many nested abstractions" in {
      val actual = Parser.abstractions("a", "b" :: "c" :: "d" :: "e" :: Nil, Var("x"))
      val expected = Abs("a", Abs("b", Abs("c", Abs("d", Abs("e", Var("x"))))))
      actual shouldEqual expected
    }

  }

  "names" should {

    def test(name: String): Unit = {
      val parser = new Parser
      parser.parseAll(parser.name, name) shouldBe a[parser.Success[_]]
    }

    def testError(name: String): Unit = {
      val parser = new Parser
      parser.parseAll(parser.name, name) shouldBe a[parser.NoSuccess]
    }

    "match a letter" in {
      test("x")
    }

    "match a name with multiple letters" in {
      test("Cappa")
    }

    "match a digit" in {
      test("0")
    }

    "match an underscore" in {
      test("_")
    }

    "match a longer name" in {
      test("__Foo_Bar_123")
    }

    "not match strings with non-word characters" in {
      testError("a-b")
      testError("x y")
      testError("!@#$%*()")
    }

    "not match the empty string" in {
      testError("")
    }

  }

  "Parser" should {

    def test(text: String, expected: Term): Unit = {
      val actual = Parser(text)
      actual.value shouldEqual expected
    }

    "parse a variable" in {
      test("a", Var("a"))
    }

    "parse a variable in parentheses" in {
      test("(a)", Var("a"))
    }

    "parse an application" in {
      test("a b", App(Var("a"), Var("b")))
    }

    "parse multiple applications" in {
      test("a b c", App(App(Var("a"), Var("b")), Var("c")))
    }

    "parse multiple applications with parentheses overriding precedence" in {
      test("a (b c)", App(Var("a"), App(Var("b"), Var("c"))))
    }

    "parse an abstraction" in {
      test("a => b", Abs("a", Var("b")))
    }

    "parse chained abstractions (unsugared)" in {
      test("a => b => c", Abs("a", Abs("b", Var("c"))))
    }

    "parse chained abstractions (sugared)" in {
      test("a b => c", Abs("a", Abs("b", Var("c"))))
    }

    "parse a let" in {
      test("a = b; c", App(Abs("a", Var("c")), Var("b")))
    }

    "parse seqential lets" in {
      test("a = b; c = d; e", App(Abs("a", App(Abs("c", Var("e")), Var("d"))), Var("b")))
    }

    "parse nested lets" in {
      test("a = b = c; d; e", App(Abs("a", Var("e")), App(Abs("b", Var("d")), Var("c"))))
    }

    "parse a parameterised let" in {
      test("a b = c; d", App(Abs("a", Var("d")), Abs("b", Var("c"))))
    }

    "parse a parameterised let with multiple arguments" in {
      test("a b c = d; e", App(Abs("a", Var("e")), Abs("b", Abs("c", Var("d")))))
    }

  }


}
