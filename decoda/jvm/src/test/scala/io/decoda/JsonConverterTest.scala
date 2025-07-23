package io.decoda

import decoda.*
import io.decoda.converter.base
import org.scalatest.funsuite.AnyFunSuite

class JsonConverterTest extends AnyFunSuite:

  given JsonCreator:
    def mkObject: base.JsonObject = new defs.JsonObject()

    def mkArray: base.JsonArray = new defs.JsonArray()

  test("native converter test") {

    case class Group(
        @JsonValue id: Int = 0,
        @JsonValue description: String = ""
    ) derives JsonConverter

    case class Person(
        @JsonValue id: Int = 0,
        @JsonValue name: String,
        @JsonValue(omitNull = true) group: Group = Group(10, "G10"),
        @JsonValue(omitNull = true) g: Option[Group] = Some(Group(20, "G20"))
    ) derives JsonConverter

    val person = Person(name = "Ricardo")
    val json = person.toJson
    val p = JsonConverter[Person].fromJson(json)
    assert(p.name == person.name)
  }
