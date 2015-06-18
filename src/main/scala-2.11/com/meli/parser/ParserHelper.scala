package com.meli.parser


object ParserHelper {

  implicit def StringToParser(exp: String): Parser[String] = new Parser[String] {
    override def apply(in: String): Result[String] =
      if (in.startsWith(exp)) Success(exp, in.drop(exp.length))
      else Failure("Wrong Word", in)
  }

}
