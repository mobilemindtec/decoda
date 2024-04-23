package codec

import br.com.mobilemind.json.codec.converter.Encoder.{field, typ, given}
import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.JsonCreator
import br.com.mobilemind.json.codec.defs
import br.com.mobilemind.json.codec.defs.|>
import org.scalatest.funsuite.AnyFunSuite

class EncoderTest extends AnyFunSuite:

  given JsonCreator with
    override def obj: base.JsonObject = defs.JsonObject()
    override def arr: base.JsonArray = defs.JsonArray()

  test("encoder test") {

    case class Person(id: Int = 0, name: String = "")

    val personEncoder =
      typ[Person]
        |> field("id", p => p.id)
        |> field("name", p => p.name)

    val jsonStr =
      personEncoder.encodeAsString(Person(1, "Ricardo"))

    val expected = """{"id": 1, "name": "Ricardo"}"""

    assert(jsonStr == expected)

  }
