package com.meli.parser

trait Parser[+A] extends (String => Result[A]) {

  def ~>[U](parser: => Parser[U]): Parser[U] = new Parser[U] {
    override def apply(input: String): Result[U] =
      Parser.this(input) match {
        case Success(value, remaining) => parser(remaining)
        case Failure(msg, inp) => Failure(msg, inp)
      }
  }

  def <~[U >: A](parser: => Parser[U]): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = Parser.this(input) match {
      case Failure(msg, in) => Failure(msg, in)
      case Success(value, remaining) =>
        val res = parser(remaining)
        res match {
          case Success(v, rem) => Success(value, rem)
          case Failure(msg, inp) => Failure(msg, inp)
        }

    }
  }

  def |[U >: A](parser: => Parser[U]): Parser[U] = new Parser[U] {
    override def apply(input: String): Result[U] = Parser.this(input) match {
      case Failure(_, _) => parser(input)
      case Success(x, n) ⇒ Success(x, n)
    }
  }

  def repeat[U](p: => Parser[U], splitter: String): Parser[List[U]] = new Parser[List[U]] {
    override def apply(in: String): Result[List[U]] = p(in) match {
      case Success(firstLetter, rest) => apply(rest).map(x => (firstLetter :: x))
      case _ if in.startsWith(splitter) => apply(in.drop(splitter.length))
      case _ => Success(Nil, in)
    }
  }

  def >>[U](fun: A => U): Parser[U] = new Parser[U] {
    def apply(in: String): Result[U] = Parser.this(in) match {
      case Success(x, rest) => Success(fun(x), rest)
      case Failure(msg, rest) => Failure(msg, rest)
    }
  }

  def ~[U](p: => Parser[U]) = new Parser[(A, U)] {
    def apply(in: String) = Parser.this(in) match {
      case Success(x, in1) =>
        p(in1) match {
          case Success(y, rest) => Success((x, y), rest)
          case Failure(msg, rest) => Failure(msg, rest)
        }
      case Failure(msg, rest) => Failure(msg, rest)
    }
  }

}

