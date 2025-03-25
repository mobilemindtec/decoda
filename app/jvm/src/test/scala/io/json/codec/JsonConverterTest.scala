package io.json.codec

import io.json.codec.infra.annotations.JsonValue
import io.json.codec.converter.base.JsonCreator
import io.json.codec.converter.auto.JsonConverter
import io.json.codec.converter.base
import org.scalatest.funsuite.AnyFunSuite

class JsonConverterTest extends AnyFunSuite:

  given JsonCreator with
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

  }
