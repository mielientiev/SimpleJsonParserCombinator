package com.meli.parser

import com.meli.parser.ParserHelper.StringToParser

object JSONParser extends TokenParser[List[JSON]] {

  override def apply(in: String): Result[List[JSON]] = repeat(jsonObject, ",")(in.trim) match {
    case Success(result, rest) if !rest.isEmpty => Failure("Couldn't Parse", rest)
    case Success(result, rest) => Success(result, rest)
    case Failure(msg, rest) => Failure(msg, rest)
  }

  private def jsonObject: Parser[JObj] = ("{" ~> repeat(jsonAttribute, ",") <~ "}") >> (x => JObj(x.toMap))

  private def jsonAttribute: Parser[(JStr, JSON)] = (jsonVal ~ ":" ~ value) >> { case ((name, ":"), value) => (name, value) }

  private def jsonVal: Parser[JStr] = "\"" ~> (stringVal >> { x => JStr(x) }) <~ "\""

  private def value: Parser[JSON] = arr | jsonObject | jsonVal | jsonNull | jsonBoolean | numberVal >> JNumb

  private def arr: Parser[JSeq] = ("[" ~> repeat(value, ",") <~ "]") >> (x => JSeq(x))

  private def jsonNull = "null" >> (x => JNull)

  private def jsonBoolean: Parser[JBoolean] = ("true" | "false") >> (x => JBoolean(x.toBoolean))

}
