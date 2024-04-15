package codec

import br.com.mobilemind.json.codec.converter.base.{DateFormatter, JsonCreator, JsonParser}
import br.com.mobilemind.json.codec.converter.{DecodeOptions, Decoder, EncodeOptions, Encoder, OptOmit}
import br.com.mobilemind.json.codec.defs.{JsonArray, JsonObject}
import codec.infra.df
import models.{Group, Person}
import codec.models.{Group, Person}
import org.scalatest.funsuite.AnyFunSuite

import java.text.SimpleDateFormat
import java.util.Date


class JsonConverterWithParserTest extends AnyFunSuite:

  given jsonParser: JsonParser with
    def parse(s: String): JsonObject = new JsonObject()

  given jsonCreator: JsonCreator with
    def empty: JsonObject = JsonObject()

  given dateFormatter: DateFormatter with
    override def format(date: Date, patter: String): String = new SimpleDateFormat(patter).format(date)

    override def parse(date: String, patter: String): Date = new SimpleDateFormat(patter).parse(date)

  given Decoder[Group] = Decoder.typ[Group]
    .string("Description", (p, v) => p.copy(description = v))
    .int("Id", (p, v) => p.copy(id = v))

  val decoder = Decoder.typ[Person]
    .string("Name", (p, v) => p.copy(name = v))
    .int("Id", (p, v) => p.copy(id = v))
    .ref[Group]("Group", (p, v) => p.copy(group = v))
    .optDate("Birthday", (p, v) => p.copy(birthday = v), Some(DecodeOptions(pattern = "yyyy-MM-dd")))
    .optInt("Age", (p, v) => p.copy(age = v))
    .optListRef[Group]("Groups", (p, v) => p.copy(groups = v))
    .listRef[Group]("Groups2", (p, v) => p.copy(groups2 = v))
    .list[Int]("PaymentDays", (p, v) => p.copy(paymentDays = v))


  given Encoder[Group] = Encoder.typ[Group]
    .int("Id", _.id)
    .string("Description", _.description)

  val encoder = Encoder.typ[Person]
    .int("Id", _.id)
    .string("Name", _.name)
    .ref("Group", _.group)
    .optDate("Birthday", _.birthday, Some(EncodeOptions(pattern = "yyyy-MM-dd")))
    .optInt("Age", _.age, Some(EncodeOptions(OptOmit.OmitNull)))
    .optListRef("Groups", _.groups)
    .listRef("Groups2", _.groups2)
    .list("PaymentDays", _.paymentDays)

  test("json decode with native parser"){
    val json = JsonObject(
      "Name" -> "Ricardo",
      "Id" -> 1,
      "Group" -> JsonObject("Id" -> 5, "Description" -> "Group 5"),
      "Birthday" -> "2010-01-05",
      "PaymentDays" -> JsonArray(15, 20, 25),
      "Groups" -> JsonArray(
        JsonObject("Id" -> 1, "Description" -> "Group 1"),
        JsonObject("Id" -> 2, "Description" -> "Group 2")
      ),
      "Groups2" -> JsonArray(
        JsonObject("Id" -> 3, "Description" -> "Group 3"),
        JsonObject("Id" -> 4, "Description" -> "Group 4")
      ))
    val p = decoder.decode(json)
  
    assert(p.name == "Ricardo")
    assert(p.id == 1)
    assert(p.group == Group(5, "Group 5"))
    assert(p.paymentDays == List(15, 20, 25))
    assert(p.groups.contains(Group(1, "Group 1") :: Group(2, "Group 2") :: Nil))
    assert(p.groups2 == Group(3, "Group 3") :: Group(4, "Group 4") :: Nil)
  }

  test("json encode with native parser") {
    val person = Person(
      id = 1,
      name = "Ricardo",
      age = Some(37),
      birthday = Some(new Date()),
      group = Group(id = 5, "Group 5"),
      paymentDays = 15 :: 22 :: 28 :: Nil,
      groups = Some(Group(id = 1, "Group 1") :: Group(id = 2, "Group 2") :: Nil),
      groups2 = Group(id = 1, "Group 1") :: Group(id = 2, "Group 2") :: Nil
    )
    val p = encoder.encode(person)

    assert(p == s"""{"Id": 1, "Name": "Ricardo", "Group": {"Id": 5, "Description": "Group 5"}, "Birthday": "${df.format(Date())}", "Age": 37, "Groups": [{"Id": 1, "Description": "Group 1"}, {"Id": 2, "Description": "Group 2"}], "Groups2": [{"Id": 1, "Description": "Group 1"}, {"Id": 2, "Description": "Group 2"}], "PaymentDays": [15, 22, 28]}""")

  }