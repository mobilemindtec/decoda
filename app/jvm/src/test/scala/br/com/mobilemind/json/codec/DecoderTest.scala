package br.com.mobilemind.json.codec

import br.com.mobilemind.json.codec.converter.*
import br.com.mobilemind.json.codec.converter.Decoder.{typ, field, given}
import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.JsonParser
import br.com.mobilemind.json.codec.defs.{Json, |>}
import org.scalatest.funsuite.AnyFunSuite

class DecoderTest extends AnyFunSuite {

  test("decoder test") {

    given JsonParser with
      override def parse(s: String): base.Json = Json.parse(s)

    case class Group(id: Int = 0, description: String = "")
    case class Person(
        id: Int = 0,
        name: String = "",
        group: Group = Group(),
        gopt: Option[Group] = None,
        gopt1: Option[Group] = None,
        ints: List[Int] = Nil,
        shorts: List[Short] = Nil,
        longs: List[Long] = Nil,
        strings: List[String] = Nil,
        doubles: List[Double] = Nil,
        floats: List[Float] = Nil,
        groups: List[Group] = Nil,
        groupsOpts: Option[List[Group]] = None,
        groupsOpts2: Option[List[Group]] = None
    )

    given Decoder[Group] =
      typ[Group]
        |> field("id", (p, i: Int) => p.copy(id = i))
        |> field("description", (p, s: String) => p.copy(description = s))

    val decoder =
      typ[Person]
        |> field("id", (p, i: Int) => p.copy(id = i))
        |> field("name", (p, s: String) => p.copy(name = s))
        |> field("group", (p, g: Group) => p.copy(group = g))
        |> field("gopt", (p, g: Option[Group]) => p.copy(gopt = g))
        |> field("gopt1", (p, g: Option[Group]) => p.copy(gopt1 = g))
        |> field("ints", (p, g: List[Int]) => p.copy(ints = g))
        |> field("longs", (p, g: List[Long]) => p.copy(longs = g))
        |> field("shorts", (p, g: List[Short]) => p.copy(shorts = g))
        |> field("doubles", (p, g: List[Double]) => p.copy(doubles = g))
        |> field("floats", (p, g: List[Float]) => p.copy(floats = g))
        |> field("strings", (p, g: List[String]) => p.copy(strings = g))
        |> field("groups", (p, g: List[Group]) => p.copy(groups = g))
        |> field("groupsOpts2", (p, g: Option[List[Group]]) => p.copy(groupsOpts2 = g))

    val person =
      decoder.parse("""
          |{
          |"id": 1,
          |"name": "Ricardo",
          |"group": {"id": 10, "description": "g10"},
          |"gopt1": {"id": 11, "description": "g11"},
          |"ints": [1, 2, 3],
          |"longs": [4, 5, 6],
          |"shorts": [7 ,8, 9],
          |"doubles": [1.1, 1.2],
          |"floats": [1.3, 1.4],
          |"strings": ["aa", "bb", "cc"],
          |"groups": [{"id": 12, "description": "g12"}, {"id": 13, "description": "g13"}],
          |"groupsOpts2": [{"id": 14, "description": "g14"}, {"id": 15, "description": "g15"}]
          |}""".stripMargin)

    val expected =
      Person(
        1,
        "Ricardo",
        Group(10, "g10"),
        None,
        Some(Group(11, "g11")),
        List(1, 2, 3),
        List(7, 8, 9),
        List(4L, 5L, 6L),
        List("aa", "bb", "cc"),
        List(1.1d, 1.2d),
        List(1.3f, 1.4f),
        List(Group(12, "g12"), Group(13, "g13")),
        None,
        Some(List(Group(14, "g14"), Group(15, "g15")))
      )
    assert(person == expected)
  }

}
