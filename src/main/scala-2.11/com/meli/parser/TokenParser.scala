package com.meli.parser

trait TokenParser[+A] extends Parser[A] {

  //Any character except quotes
  def stringVal: Parser[String] = new Parser[String] {
    override def apply(in: String): Result[String] = {
      val (word, rest) = in.span(_ != '\"')
      Success(word, rest)
    }
  }

  def numberVal: Parser[Double] = new Parser[Double] {
    override def apply(input: String): Result[Double] = {
      """(^[-+]?[0-9]*\.?[0-9]+)""".r.findFirstMatchIn(input) match {
        case Some(regex) if regex.start == 0 => Success(regex.group(0).toDouble, input.drop(regex.group(0).length))
        case _ => Failure("It's not number", input)
      }
    }
  }

}
