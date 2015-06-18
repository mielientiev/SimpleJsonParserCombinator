package com.meli.parser

import org.scalatest.FunSuite

class JSONParserTest extends FunSuite {

  test("An empty string should return empty list") {
    val result: Result[List[JSON]] = JSONParser("")
    assert(!result.isFailure)
    assert(result.get == List())
  }

  test("An empty json obj should return empty List JObj") {
    val result: Result[List[JSON]] = JSONParser("{}")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map())))
  }

  test("simple key:value json") {
    val result: Result[List[JSON]] = JSONParser( """{"abc":"eee"}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("abc") -> JStr("eee")))))
  }

  test("simple list of key:value json") {
    val result: Result[List[JSON]] = JSONParser( """{"abc":"eee","rrr":"777"}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("abc") -> JStr("eee"), JStr("rrr") -> JStr("777")))))
  }

  test("nested json") {
    val result: Result[List[JSON]] = JSONParser( """{"abc":"eee","rrr":{"999":"777","ppp":"ooo"}}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("abc") -> JStr("eee"), JStr("rrr") -> JObj(Map(JStr("999") -> JStr("777"), JStr("ppp") -> JStr("ooo")))))))
  }

  test("json with booleans") {
    val result: Result[List[JSON]] = JSONParser( """{"abc":"eee","rrr":true,"second":false}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("abc") -> JStr("eee"), JStr("rrr") -> JBoolean(true), JStr("second") -> JBoolean(false)))))
  }

  test("json with null") {
    val result: Result[List[JSON]] = JSONParser( """{"abc":null}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("abc") -> JNull))))
  }

  test("json with array elements") {
    val result: Result[List[JSON]] = JSONParser( """{"%ee":["abc","www"]}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("%ee") -> JSeq(List(JStr("abc"), JStr("www")))))))
  }

  test("json with an empty array") {
    val result: Result[List[JSON]] = JSONParser( """{"ee":[]}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("ee") -> JSeq(List())))))
  }

  test("json with integer") {
    val result: Result[List[JSON]] = JSONParser( """{"ee":5545.5,"www":0}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("ee") -> JNumb(5545.5), JStr("www") -> JNumb(0)))))
  }

  test("complex list of json ") {
    val result: Result[List[JSON]] = JSONParser( """{"abc":null,"eee":{"11":true,"112":null,"112444":["1","2",{"55":"ww"},{"787":"88","87878":"88"}]}},{"abc":null,"eee":{"11":true,"112":null,"112444":["1","2",{"55":"ww"},{"787":"88","87878":"88"}]}}""")
    assert(!result.isFailure)
    assert(result.get == List(JObj(Map(JStr("abc") -> JNull, JStr("eee") -> JObj(Map(JStr("11") -> JBoolean(true), JStr("112") -> JNull, JStr("112444") -> JSeq(List(JStr("1"), JStr("2"), JObj(Map(JStr("55") -> JStr("ww"))), JObj(Map(JStr("787") -> JStr("88"), JStr("87878") -> JStr("88"))))))))),
      JObj(Map(JStr("abc") -> JNull, JStr("eee") -> JObj(Map(JStr("11") -> JBoolean(true), JStr("112") -> JNull, JStr("112444") -> JSeq(List(JStr("1"), JStr("2"), JObj(Map(JStr("55") -> JStr("ww"))), JObj(Map(JStr("787") -> JStr("88"), JStr("87878") -> JStr("88")))))))))))
  }
}
