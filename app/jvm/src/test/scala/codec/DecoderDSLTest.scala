package codec

import org.scalatest.funsuite.AnyFunSuite
import br.com.mobilemind.json.codec.converter.{Decoder, base}
import br.com.mobilemind.json.codec.converter.base.JsonCreator
import br.com.mobilemind.json.codec.defs

class DecoderDSLTest extends AnyFunSuite:

  case class Group(id: Int = 0, description: String = "")

  case class Person(
      id: Int = 0,
      name: String = "",
      group: Option[Group] = None,
      ints: List[Int] = Nil,
      groups: List[Group] = Nil,
      groupsOpt: Option[List[Group]] = None
  )

  given JsonCreator with
    override def empty: base.JsonObject = new defs.JsonObject()

  test("test deconder dsl") {

    import Decoder.dsl.*

    given Decoder[Group] =
      of[Group]
        |> field("id", (g, id: Int) => g.copy(id = id))
        |> field("description", (g, s: String) => g.copy(description = s))

    val personDecoder =
      of[Person]
        |> field("id", (p, id: Int) => p.copy(id = id))
        |> ref("group", (p, g: Group) => p.copy(group = Some(g)))
        |> list("ints", (p, l: List[Int]) => p.copy(ints = l))
        |> listRef("groups", (p, l: List[Group]) => p.copy(groups = l))
        |> listRef(
          "groupsOpt",
          (p, l: List[Group]) => p.copy(groupsOpt = Some(l))
        )
  }
