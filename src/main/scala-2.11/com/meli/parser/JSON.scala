package com.meli.parser

sealed abstract class JSON

case class JSeq(elems: List[JSON]) extends JSON

case class JObj(obj: Map[JStr, JSON]) extends JSON

case class JNumb(numb: Double) extends JSON

case class JBoolean(result: Boolean) extends JSON

case class JStr(string: String) extends JSON

case object JNull extends JSON
